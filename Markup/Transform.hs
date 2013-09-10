module Markup.Transform ( Transform, transformElem, transformContent
                        , Analysis, analyzeElem, analyzeContent )
where

import Control.Monad

import Markup.AST

type Transform m = Content -> m [Content]

transformContent :: Monad m => Transform m -> Content -> m [Content]
transformContent trans t@(Text _) = trans t
transformContent trans (Child e) = transformElem trans e

transformElem :: Monad m => Transform m -> Elem -> m [Content]
transformElem trans (Elem tag attrs children) = do
  transChildren <- liftM concat $ mapM (transformContent trans) children
  trans (Child (Elem tag attrs transChildren))

type Analysis m = Content -> m ()

analyzeContent :: Monad m => Analysis m -> Content -> m ()
analyzeContent analyze c = do transformContent trans c; return ()
    where trans c = do analyze c; return [c]

analyzeElem :: Monad m => Analysis m -> Elem -> m ()
analyzeElem analyze c = do transformElem trans c; return ()
    where trans c = do analyze c; return [c]
