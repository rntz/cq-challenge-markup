module Markup.AST ( Doc (..), docToElem, Elem (..), Content (..) )
where

newtype Doc = Doc { docContents :: [Content] }
    deriving (Show, Eq, Ord)

data Elem = Elem { elemTag :: String
                 , elemAttrs :: [(String,String)]
                 , elemContent :: [Content] }
            deriving (Show, Eq, Ord)

data Content = Text String | Child Elem
               deriving (Show, Eq, Ord)

-- Adds the "body" tag that the markup spec says we should automatically add.
-- Sometimes it's useful not to add it, though (like when a markup document's
-- HTML interpretation is to be included in another HTML document).
docToElem :: Doc -> Elem
docToElem (Doc content) = Elem "body" [] content
