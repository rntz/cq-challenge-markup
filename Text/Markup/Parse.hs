module Text.Markup.Parse
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

import Text.Markup.AST
import Text.Markup.XML          -- XXX: remove

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
parse :: Config -> SourceName -> String -> Either ParseError Elem
parse cfg sourceName text = parse1 cfg sourceName document text

parse1 cfg sourceName p text = runReader parsed ctx
    where parsed = runPT p () sourceName text
          ctx = Context { ctxIndentDepth = 0, ctxConfig = cfg }

test1 p s = case Text.Markup.Parse.parse1 defaultConfig "<unknown>" p s of
              Right x -> x
              Left e -> error (show e)

test p s = putStrLn $ showMarkupAsXML $ test1 p s


-- Miscellany
askMetachars :: Parser [Char]
askMetachars = chars <$> askConfig parseLinks
    where chars links = "\r\n\\{}" ++ if links then "[]" else ""

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
            | x < 0 = mzero      -- fail
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


-- Parsing documents
document :: Parser Elem
document = Elem "body" <$> subdocument <* eof

subdocument :: Parser [Content]
subdocument = map Child <$> indentedBlankSep element
    where element = section <|> header <|> paragraph
          section = indented 2 $ indented 1 verbatim <|> list <|> blockquote


-- Lists, blockquotes, verbatims
list :: Parser Elem
list = do (name, eltChar) <- lookAhead (start "ol" '#' <|> start "ul" '-')
          Elem name . map Child <$> indentedBlankSep (listElt eltChar)
    where
      start name eltChar = (name, eltChar) <$ char eltChar
      listElt eltChar = do char eltChar; indent 1
                           Elem "li" <$> deepened 2 subdocument

blockquote :: Parser Elem
blockquote = Elem "blockquote" <$> subdocument

verbatim = do head <- line
              rest <- (newline >> moreLines) <|> ([] <$ eof)
              -- Trailing blank lines are omitted
              let lines = dropTrailingBlanks (head : rest)
              return $ Elem "pre" [Text (intercalate "\n" lines)]
    where
      line = manyTill anyChar eol -- gobble a line of verbatim input
      -- We try blankLine before indentedLine to ensure that blank lines become
      -- empty strings (""), so are dropped correctly by dropTrailingBlanks.
      moreLines = sepEndBy (blankLine <|> indentedLine) newline
      blankLine = "" <$ try (inlineSpaces <* eol)
      indentedLine = currentIndent >> line
      -- Must use eof instead of eod, because verbatim sections in subdocuments
      -- can contain right-braces }.
      eol = lookAhead newline <|> eof <?> "end of line"
      dropTrailingBlanks = reverse . dropWhile (== "") . reverse



-- Paragraphs

{- A "chunk" a simple in-line component of a text span. It is one of:
    - A sequence of unescaped characters ("foo *bar*, baz")
    - An escaped character ("\{")
    - An explicitly tagged element ("\i{whatever}") 
 -}
chunk :: Parser Content
chunk = (Text <$> (many1 . noneOf =<< askMetachars)) <|> -- plain old text
        (char '\\' >> (taggedElement <|> escapedChar))
    where
      -- the (:[]) turns a Char into a [Char], ie a String.
      -- XXX: '-' can't be escaped with backslash. This is due to an issue with
      -- the markup spec.
      escapedChar = Text . (:[]) <$> satisfy (not . isTagChar)
      taggedElement = Child <$> do
        isSubdocTag <- askConfig isSubdocumentTag
        name <- many1 (satisfy isTagChar)
        contents <- between (char '{') (char '}') $
                    if isSubdocTag name then subdocument else span
        return $ Elem name contents

-- One line in a paragraph or text span
spanLine :: Parser [Content]
spanLine = many1 chunk

-- A text span: the body of a paragraph or non-subdocument tagged element.
span :: Parser [Content]
-- (intercalate [Text " "]) adds spaces between lines, as per the Markup spec
span = joinText . intercalate [Text " "] <$> sepEndBy spanLine nextLine
    where
      joinText (Text x : Text y : rest) = joinText $ Text (x++y) : rest
      joinText (x:xs) = x : joinText xs
      joinText [] = []
      -- A line-ending followed by the appropriate amount of whitespace to bring us
      -- back to our current indent level.
      nextLine = try $ newline >> currentIndent

-- A paragraph can't start with an unescaped *, but that's handled by trying to
-- parse header before us.
paragraph :: Parser Elem
paragraph = Elem "p" <$> nonEmpty "empty paragraph" span

header :: Parser Elem
header = do hlvl <- length <$> many1 (char '*') <* char ' '
            Elem ("h" ++ show hlvl) <$> nonEmpty "empty header" span
