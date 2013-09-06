module Markup.Sexp ( Sexp (..)
                   , markupToSexp, contentToSexp, showMarkupAsSexp )
where

import Data.List (intercalate)

import Markup.AST

data Sexp = Atom String
          | String String
          | List [Sexp]
            deriving (Eq, Ord)

instance Show Sexp where
    show s = pretty 0 s
        where pretty n (Atom s) = s
              pretty n (String s) = show s
              pretty n (List s) = "(" ++ intercalate sep body ++ ")"
                  where body = map (pretty (n+1)) s
                        sep | all atomic s = " "
                            | otherwise = "\n" ++ replicate (n+1) ' '
                        atomic (List _) = False
                        atomic _ = True

markupToSexp :: Elem -> Sexp
contentToSexp :: Content -> Sexp

markupToSexp (Elem tag contents) = List (Atom tag : map contentToSexp contents)
contentToSexp (Text s) = String s
contentToSexp (Child elem) = markupToSexp elem

-- TODO: pretty-printing
showMarkupAsSexp :: Elem -> String
showMarkupAsSexp = show . markupToSexp
