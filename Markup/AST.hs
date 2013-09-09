module Markup.AST ( Elem (..), Content (..) )
where

data Elem = Elem { elemTag :: String
                 , elemAttrs :: [(String,String)]
                 , elemContent :: [Content] }
            deriving (Show, Eq, Ord)

data Content = Text String | Child Elem
               deriving (Show, Eq, Ord)
