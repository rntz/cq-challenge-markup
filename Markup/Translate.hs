module Markup.Translate ( Translation, translateElem, translateContent
                        , Analysis, analyzeElem, analyzeContent )
where

import Control.Monad

import Markup.AST

type Translation m = Content -> m [Content]

translateContent :: Monad m => Translation m -> Content -> m [Content]
translateContent trans t@(Text _) = trans t
translateContent trans (Child e) = translateElem trans e

translateElem :: Monad m => Translation m -> Elem -> m [Content]
translateElem trans (Elem tag attrs children) = do
  transChildren <- liftM concat $ mapM (translateContent trans) children
  trans (Child (Elem tag attrs transChildren))

type Analysis m = Content -> m ()

analyzeContent :: Monad m => Analysis m -> Content -> m ()
analyzeContent analyze c = do translateContent trans c; return ()
    where trans c = do analyze c; return [c]

analyzeElem :: Monad m => Analysis m -> Elem -> m ()
analyzeElem analyze c = do translateElem trans c; return ()
    where trans c = do analyze c; return [c]
