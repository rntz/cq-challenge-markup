module Markup.Parse
where

import Prelude hiding (span)    -- we want to use that name

import Control.Monad.Reader
import Data.Char (isSpace, isAlphaNum)
import Data.List (intercalate)
import Text.Parsec hiding (newline) -- Parsec's newline is '\n', but we want
                                    -- '\r\n' and '\r' also.

import Control.Applicative hiding
    -- These are already provided by Text.Parsec, so to avoid duplicates we
    -- avoid them here.
    ((<|>), optional, many)

import Markup.AST

-- External configuration passed to the parser
data Config = Config {
    -- predicate telling us which tags start subdocuments
      isSubdocumentTag :: String -> Bool
    -- whether to parse links
    , parseLinks :: Bool
    }

defaultConfig = Config { isSubdocumentTag = const False
                       , parseLinks = True }

-- Our internal context while parsing.
data Context = Context { ctxIndentDepth :: Int -- current indentation depth
                       , ctxConfig :: Config } -- configuration

-- We parse strings, we have no state, and our underlying monad is the ability
-- to read our context.
type Parser a = ParsecT String () (Reader Context) a

-- Getting at our configuration from context.
askConfig :: (Config -> a) -> Parser a
askConfig accessor = asks (accessor . ctxConfig)

-- The parse function
parse :: Config -> SourceName -> String -> Either ParseError Doc
parse cfg sourceName text = parse1 cfg sourceName document text

parse1 cfg sourceName p text = runReader parsed ctx
    where parsed = runPT p () sourceName text
          ctx = Context { ctxIndentDepth = 0, ctxConfig = cfg }


-- Miscellany
askMetachars :: Parser [Char]
askMetachars = chars <$> askConfig parseLinks
    where chars links = "\r\n\\{}" ++ if links then "[" else ""

isTagChar x = isAlphaNum x || elem x "_.+"

nonEmpty :: String -> Parser [a] -> Parser [a]
nonEmpty s p = do l <- p
                  if null l then fail s else return l

ignore p = () <$ p              -- Performs p, then returns ().

newline = (ignore $ char '\n') <|> (char '\r' >> optional (char '\n'))
          <?> "newline"

eod = eof <|> ignore (lookAhead $ char '}') <?> "end of (sub)document"

isInlineSpace x = isSpace x && notElem x "\n\r"
inlineSpace = satisfy isInlineSpace <?> "inline space"
inlineSpaces = skipMany inlineSpace <?> "inline white-space"

-- Gobbles blank lines until a non-blank line or end-of-document. Always
-- succeeds.
-- XXX: there has to be a better way to do this.
blankLines = do many $ try $ inlineSpaces >> newline
                optional $ try $ inlineSpaces >> eod


-- Indentation
-- Accepts indentation up to the given depth. Never fails hard.
indent :: Int -> Parser ()
indent x = try $ ind x
    where
      ind x | x > 0 = do c <- inlineSpace
                         ind (x - charWidth c)
            | x == 0 = return () -- succeed
            | otherwise = mzero  -- fail
      -- Width of a character for depth-of-indentation purposes.
      charWidth '\t' = 8
      charWidth _ = 1

-- Accepts indentation up to the current indent depth.
currentIndent :: Parser ()
currentIndent = asks ctxIndentDepth >>= indent

-- Runs another parser at a deeper indentation level.
deepened :: Int -> Parser a -> Parser a
deepened i = local (\ctx -> ctx { ctxIndentDepth = i + ctxIndentDepth ctx})

-- The same, but conveniently parses the indent first.
indented :: Int -> Parser a -> Parser a
indented i p = indent i >> deepened i p

-- Separated by blank lines and indented to the current depth
indentedBlankSep :: Parser a -> Parser [a]
indentedBlankSep p = sepBy (p <* blankLines) currentIndent

endOfLine = lookAhead newline <|> eof <?> "end of line"
restOfLine = manyTill anyChar endOfLine


-- Parsing documents
document :: Parser Doc
document = Doc <$> (optional modeline >> subdocument <* eof)

modeline :: Parser ()
-- TODO: avoid use of try here
modeline = ignore $ do try (inlineSpaces >> string "-*-"); restOfLine

-- TODO: why does (element = blankLines >> ...) when it's being used in
-- blankSep?
subdocument :: Parser [Content]
subdocument = map Child <$> indentedBlankSep element
    where element = blankLines >> (section <|> header <|> paragraph)
          section = indented 2 $ indented 1 verbatim <|> list <|> blockquote


-- Lists, blockquotes, verbatims
list :: Parser Elem
list = do (name, eltChar) <- lookAhead (start "ol" '#' <|> start "ul" '-')
          Elem name [] . map Child <$> indentedBlankSep (listElt eltChar)
    where
      start name eltChar = (name, eltChar) <$ char eltChar
      listElt eltChar = do char eltChar; indent 1
                           Elem "li" [] <$> deepened 2 subdocument

blockquote :: Parser Elem
blockquote = Elem "blockquote" [] <$> subdocument

verbatim = do head <- restOfLine
              -- Must use eof instead of eod, because verbatim sections in
              -- subdocuments can contain right-braces }.
              rest <- (newline >> moreLines) <|> ([] <$ eof)
              -- Trailing blank lines are omitted
              let lines = dropTrailingBlanks (head : rest)
              return $ Elem "pre" [] [Text (intercalate "\n" lines)]
    where
      -- We try blankLine before indentedLine to ensure that blank lines become
      -- empty strings (""), so are dropped correctly by dropTrailingBlanks.
      moreLines = sepEndBy (blankLine <|> indentedLine) newline
      blankLine = "" <$ try (inlineSpaces <* endOfLine)
      indentedLine = currentIndent >> restOfLine
      dropTrailingBlanks = reverse . dropWhile (== "") . reverse


-- Headers and Paragraphs
textChild :: String -> String -> Content
textChild x y = childElem x [] [Text y]

header :: Parser Elem
header = do hlvl <- length <$> many1 (char '*') <* char ' '
            Elem ("h" ++ show hlvl) [] <$> nonEmpty "empty header" span

-- A paragraph can't start with an unescaped *, but that's handled by trying to
-- parse header before us.
paragraph :: Parser Elem
paragraph = linkDef <|> Elem "p" [] <$> nonEmpty "empty paragraph" span

-- A text span: the body of a paragraph or non-subdocument tagged element.
span :: Parser [Content]
span = spanExcept ""

-- A span which treats certain characters as metachars in addition to the usual
-- ones.
spanExcept :: String -> Parser [Content]
-- (intercalate [Text " "]) adds spaces between lines, as per the Markup spec
spanExcept metas = joinText . intercalate [Text " "] <$>
                   sepEndBy spanLine nextLine
    where
      -- One line in a paragraph or text span
      spanLine = many1 (chunk metas)
      -- A line-ending followed by the appropriate amount of whitespace to bring
      -- us back to our current indent level.
      nextLine = try $ newline >> currentIndent
      -- Joins adjacent Text elements together, just to simplify things.
      joinText (Text x : Text y : rest) = joinText $ Text (x++y) : rest
      joinText (x:xs) = x : joinText xs
      joinText [] = []

{- A "chunk" a simple in-line component of a text span. It is one of:
    - A sequence of unescaped non-meta characters ("foo *bar*, baz")
    - An escaped character ("\{")
    - An explicitly tagged element ("\i{whatever}")

   We extend the markup spec by allowing tagged elements to have attributes
   specified using \foo[attr1=value1,attr2=value2] notation.

   Values and attributes may be any sequence of tag-characters, or quoted
   strings. Inside quoted strings the escapes \\ and \" are understood, but no
   others.
 -}
chunk :: String -> Parser Content
chunk metas = -- plain old text
              (Text <$> (many1 . noneOf . (metas ++) =<< askMetachars)) <|>
              -- a link
              (do pl <- askConfig parseLinks
                  guard pl
                  Child <$> between (char '[') (char ']') linkContent) <|>
              -- escaped character
              (char '\\' >> (taggedElement <|> escapedChar))
    where
      -- XXX: '-' can't be escaped with backslash. This is due to an issue with
      -- the markup spec. TODO: fix by saying that tag names can't start with -?
      escapedChar = Text . (:[]) <$> satisfy (not . isTagChar)
      taggedElement = Child <$> do
        isSubdocTag <- askConfig isSubdocumentTag
        name <- tagName
        attrs <- option [] $ between (char '[') (char ']') $
                 sepEndBy attr (char ',')
        contents <- between (char '{') (char '}') $
                    if isSubdocTag name then subdocument else span
        return $ Elem name attrs contents
      tagName = many1 (satisfy isTagChar)
      spaced = between inlineSpaces inlineSpaces
      attr = spaced $ do
        name <- tagName <|> quotedString
        spaced $ char '='
        value <- tagName <|> quotedString
        return (name, value)
      quotedString = between (char '"') (char '"') $ many stringChar
      stringChar = noneOf "\"\\" <|> (char '\\' >> oneOf "\"\\")

linkContent :: Parser Elem
linkContent = do content <- spanExcept "|]"
                 -- FIXME: do we need to gobble whitespace here?
                 key <- option [] $ do char '|'
                                       name <- many (noneOf "]")
                                       return [textChild "key" name]
                 return $ Elem "link" [] (content ++ key)

linkDef :: Parser Elem
linkDef = try $ do name <- between (char '[') (char ']') $ many $ noneOf "]"
                   inlineSpaces
                   url <- between (char '<') (char '>') $ many $ noneOf ">"
                   return $ Elem "link_def" [] [textChild "link" name,
                                                textChild "url" url]
