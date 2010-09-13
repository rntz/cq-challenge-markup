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

-- Our context while parsing.
data Context = Context {
    -- current indentation depth
      ctxIndentDepth :: Int
    -- predicate determining which tags are subdocument
    , ctxIsSubdocumentTag :: String -> Bool
    }

-- For simplicity and readability, especially to those not familiar with
-- Haskell, and to avoid having to use GHC extensions, we are limiting ourselves
-- to parsing strings.
type Parser a = ParsecT String () (Reader Context) a

-- The parse function
parse :: (String -> Bool) -> SourceName -> String -> Either ParseError Elem
parse isSubdocTag sourceName text = parse1 isSubdocTag sourceName (document <* eof) text

parse1 isSubdocTag sourceName p text = runReader parsed ctx
    where parsed = runPT p () sourceName text
          ctx = Context { ctxIndentDepth = 0
                        , ctxIsSubdocumentTag = isSubdocTag }

test1 p s = case Text.Markup.Parse.parse1 (const True) "<unknown>" p s of
              Right x -> x
              Left e -> error (show e)

test p s = putStrLn $ showMarkupAsXML $ test1 p s


-- Miscellany
metachars = "\r\n\\{}"
isTagChar x = isAlphaNum x || elem x "-.+"

-- Checks that the next character isn't "empty" - that is, either whitespace, an
-- end-of-document indicator '}', or EOF.
notEmpty = lookAhead $ satisfy $ \x -> not (isSpace x || x == '}')

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
blankLines = do many (try $ inlineSpaces >> newline)
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
indented i p = do indent i; deepened i p

-- Separated by blank lines and indented to the current depth
indentedBlankSep :: Parser a -> Parser [a]
indentedBlankSep p = sepBy (p <* blankLines) currentIndent


-- Parsing documents
document :: Parser Elem
document = Elem "body" <$> subdocument

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
chunk = (Text <$> many1 (noneOf metachars)) <|> -- plain old text
        (char '\\' >> (taggedElement <|> escapedChar))
    where
      -- the (:[]) turns a Char into a [Char], ie a String.
      -- XXX: '-' can't be escaped with backslash. This is due to an issue with
      -- the markup spec.
      escapedChar = Text . (:[]) <$> satisfy (not . isTagChar)
      taggedElement = Child <$> do
        isSubdocTag <- asks ctxIsSubdocumentTag
        name <- many1 $ satisfy isTagChar
        contents <- between (char '{') (char '}') $
                    if isSubdocTag name then subdocument else span
        return $ Elem name contents

-- One line in a paragraph or text span
spanLine :: Parser [Content]
spanLine = many chunk           -- having zero chunks is possible, eg. \i{}

-- A text span: the body of a paragraph or non-subdocument tagged element.
span :: Parser [Content]
-- (intercalate [Text " "]) adds spaces between lines, as per the Markup spec
span = joinText . intercalate [Text " "] <$> sepBy spanLine nextLine
    where
      joinText (Text x : Text y : rest) = joinText $ Text (x++y) : rest
      joinText (x:xs) = x : joinText xs
      joinText [] = []
      -- A line-ending followed by the appropriate amount of whitespace to bring us
      -- back to our current indent level.
      nextLine = try $ newline >> currentIndent >> notEmpty

-- A paragraph can't begin with a space, but that should be ensured by our
-- caller. Similarly, can't start with an unescaped *, but that's handled by
-- trying to parse header before us.
paragraph :: Parser Elem
paragraph = notEmpty >> Elem "p" <$> span

header :: Parser Elem
header = do hlvl <- length <$> many1 (char '*') <* char ' '
            Elem ("h" ++ show hlvl) <$> span
