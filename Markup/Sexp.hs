module Markup.Sexp ( Sexp (..)
                   , markupToSexp, contentToSexp, showMarkupAsSexp )
where

import Data.List (intercalate)
import Data.Char (isPrint, ord)
import Numeric (showHex)

import Markup.AST

data Sexp = Atom String
          | String String
          | List [Sexp]
            deriving (Eq, Ord)

instance Show Sexp where
    show s = pretty 0 s
        where pretty n (Atom s) = s
              pretty n (String s) = '"' : escapeString s ++ "\""
              pretty n (List s) = "(" ++ intercalate sep body ++ ")"
                  where body = map (pretty (n+1)) s
                        sep | all atomic s = " "
                            | otherwise = "\n" ++ replicate (n+1) ' '
                        atomic (List _) = False
                        atomic _ = True

-- Escapes a string in the manner understood by Racket Scheme.
escapeString s = concatMap escapeChar s
    where escapeChar '"' = "\\\""
          escapeChar '\\' = "\\\\"
          escapeChar c | isPrint c = [c]
          escapeChar c = res
              where n = ord c
                    hex = showHex n ""
                    res | n <= 0xff = "\\x" ++ pad 2 hex
                        | n <= 0xffff = "\\u" ++ pad 4 hex
                        | otherwise = "\\U" ++ pad 8 hex
                    pad n s = replicate (n - length s) '0' ++ s

markupToSexp :: Elem -> Sexp
contentToSexp :: Content -> Sexp

-- TODO: currently we just drop attrs. maybe we should include them?
markupToSexp (Elem tag _ contents) =
    List (Atom tag : map contentToSexp contents)
contentToSexp (Text s) = String s
contentToSexp (Child elem) = markupToSexp elem

-- TODO: pretty-printing
showMarkupAsSexp :: Elem -> String
showMarkupAsSexp = show . markupToSexp
