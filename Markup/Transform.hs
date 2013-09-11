module Markup.Transform
    ( Transform, transformDoc, transformElem, transformContent
    , transformContents
    , Analysis, analyzeDoc, analyzeElem, analyzeContent, analyzeContents )
where

import Control.Monad

import Markup.AST

type Transform m = Elem -> m [Content]

transformDoc :: Monad m => Transform m -> Doc -> m Doc
transformElem :: Monad m => Transform m -> Elem -> m [Content]
transformContent :: Monad m => Transform m -> Content -> m [Content]
transformContents :: Monad m => Transform m -> [Content] -> m [Content]

transformDoc trans (Doc contents) = liftM Doc $ transformContents trans contents
transformContents trans = liftM concat . mapM (transformContent trans)

transformContent trans (Child e) = transformElem trans e
transformContent trans t@(Text _) = return [t]

transformElem trans (Elem tag attrs content) = do
  newContent <- transformContents trans content
  trans (Elem tag attrs newContent)

type Analysis m = Elem -> m ()

analyzeDoc :: Monad m => Analysis m -> Doc -> m ()
analyzeElem :: Monad m => Analysis m -> Elem -> m ()
analyzeContent :: Monad m => Analysis m -> Content -> m ()
analyzeContents :: Monad m => Analysis m -> [Content] -> m ()

analyzeDoc analyze (Doc contents) = analyzeContents analyze contents
analyzeContents analyze es = do transformContents (lift analyze) es; return ()
analyzeContent analyze e = do transformContent (lift analyze) e; return ()
analyzeElem analyze e = do transformElem (lift analyze) e; return ()

lift :: Monad m => Analysis m -> Transform m
lift analyze e = do analyze e; return [Child e]
