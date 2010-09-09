module Text.Markup.Parse
where

import Prelude hiding (span)    -- we want to use that name

import Control.Monad.Reader
import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Parsec hiding (newline) -- Parsec's newline is '\n', but we want
                                    -- '\r\n' and '\r' also.

import Control.Applicative hiding 
    -- These are already provided by Text.Parsec, so to avoid duplicates we
    -- avoid them here.
    ((<|>), optional, many)

import Text.Markup.AST

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

metachars = "\r\n\\{}"

isInlineSpace x = Data.Char.isSpace x && notElem x "\n\r"

noSpace = notFollowedBy space

inlineSpace = satisfy isInlineSpace <?> "inline space"
inlineSpaces = skipMany inlineSpace <?> "inline white-space"

newline = (() <$ char '\n') <|> (char '\r' >> optional (char '\n'))
          <?> "newline"

eol = eof <|> newline

blankLine = eof <|> (newline >> inlineSpaces >> eol) <?> "blank line"

blankLines = eof <|> 


-- Indentation
-- Width of a character for depth-of-indentation purposes.
charWidth '\t' = 8
charWidth _ = 1

-- Accepts indentation up to the given depth.
indent :: Int -> Parser ()
indent i | i > 0 = do c <- inlineSpace
                      indent (i - charWidth c)
         | i == 0 = return ()
         | i < 0 = mzero

-- Accepts indentation up to the current indent depth.
currentIndent :: Parser ()
currentIndent = asks ctxIndentDepth >>= indent

-- Runs another parser at a deeper indentation level.
deepened :: Int -> Parser a -> Parser a
deepened i = local (\ctx -> ctx { ctxIndentDepth = i + ctxIndentDepth ctx})

-- The same, but conveniently parses the indent first.
indented :: Int -> Parser a -> Parser a
indented i p = do indent i; deepened i p


-- Parsing documents
document :: Parser Elem
document = Elem "body" <$> subdocument

subdocument :: Parser [Content]
subdocument = map Child <$> sepBy element blankLines
    where element = section <|> paragraph
          section = indented 2 $ (indented 1 verbatim) <|> list <|> blockquote


-- Lists, blockquotes, verbatims
list :: Parser Elem
list = do (name, eltChar) <- lookAhead (start "ol" '#' <|> start "ul" '-')
          -- Are list elements necessarily separated by blank lines?
          Elem name <$> sepBy (listElt eltChar) blankLines
    where
      start name eltChar = (name, eltChar) <$ char eltChar
      listElt eltChar = do char eltChar; indent 1
                           Elem "li" <$> deepened 2 subdocument

blockquote :: Parser Elem
blockquote = Elem "blockquote" <$> subdocument

verbatim :: Parser Elem
verbatim = wrap . intercalate "\n" <$> sepBy line (blankLines >> nextLine)
    where
      line = manyTill anyChar (lookAhead newline <|> eof)
      wrap text = Elem "verbatim" [Text e]



-- Paragraphs

-- A line-ending followed by the appropriate amount of whitespace to bring us
-- back to our current indent level.
nextLine = try $ newline >> currentIndent >> noSpace

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
      escapedChar = Text . (:[]) <$> anyChar
      taggedElement = Child <$> do
        isSubdocTag <- asks ctxIsSubdocumentTag
        name <- many1 (alphaNum <|> oneOf "-.+")
        contents <- if isSubdocTag name then subdocument else span
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

-- A paragraph can't begin with a space, but that should be ensured by our
-- caller. Similarly, can't start with an unescaped *, but that's handled by
-- trying to parse header before us.
paragraph :: Parser Elem
paragraph = Elem "p" <$> span
