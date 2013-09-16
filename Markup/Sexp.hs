module Markup.Sexp ( Sexp (..)
                   , docToSexp, elemToSexp, contentToSexp
                   , showSexp )
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

showSexp :: Sexp -> String
showSexp = show

docToSexp :: Bool -> Doc -> Sexp
elemToSexp :: Bool -> Elem -> Sexp
contentToSexp :: Bool -> Content -> Sexp

docToSexp withAttrs (Doc content) = List $ map (contentToSexp withAttrs) content

elemToSexp withAttrs (Elem tag as cs)
    = List (car : map (contentToSexp withAttrs) cs)
      where car = if not withAttrs then Atom tag
                  else List (Atom tag : [List [Atom a, Atom v] | (a,v) <- as])

contentToSexp _ (Text s) = String s
contentToSexp withAttrs (Child elem) = elemToSexp withAttrs elem
