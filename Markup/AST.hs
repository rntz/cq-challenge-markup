module Markup.AST ( Doc (..), Attrs, Elem (..), Content (..)
                  , docToElem, childElem)
where

newtype Doc = Doc { docContents :: [Content] }
    deriving (Show, Eq, Ord)

type Attrs = [(String,String)]

data Elem = Elem { elemTag :: String
                 , elemAttrs :: Attrs
                 , elemContent :: [Content] }
            deriving (Show, Eq, Ord)

data Content = Text String | Child Elem
               deriving (Show, Eq, Ord)

-- Adds the "body" tag that the markup spec says we should automatically add.
-- Sometimes it's useful not to add it, though (like when a markup document's
-- HTML interpretation is to be included in another HTML document).
docToElem :: Doc -> Elem
docToElem (Doc content) = Elem "body" [] content

childElem tag attrs content = Child $ Elem tag attrs content
