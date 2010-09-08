module Text.Markup.Ast ( Markup (..) )
where

data Markup = Element { eltTag :: String, eltChildren :: [Markup] }
            | Text String
              deriving (Show, Eq, Ord)
