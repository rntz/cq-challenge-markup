module Markup.Html ( module Markup.AST
                   , markupToHtml, showHtml, renderMarkupAsHtml )
where

import Control.Monad.Identity
import Control.Monad.Writer

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Html

import Markup.AST
import Markup.Transform

-- renderHTML doesn't fuck up whitespace inside of tags where it matters,
-- although it does produce HTML that's ugly as hell.
showHtml :: Html -> String
showHtml = renderHtml

renderMarkupAsHtml :: Elem -> String
renderMarkupAsHtml = showHtml . markupToHtml

htmlAttrs :: [(String,String)] -> [HtmlAttr]
htmlAttrs = map (uncurry strAttr)

instance HTML Elem where
    toHtml (Elem tagname attrs children) =
        tag tagname ! map (uncurry strAttr) attrs $ toHtmlFromList children

instance HTML Content where
    -- TODO: toHtml for Strings assumes that the content-encoding of the page is
    -- going to be utf8. is there some way to fix this?
    toHtml (Text s) = toHtml s
    toHtml (Child e) = toHtml e

markupToHtml :: Elem -> Html
markupToHtml body = toHtml newBody
    where newBody = runIdentity $ transformElem (fixLinks defs) body
          defs = Map.fromList $ execWriter $ analyzeElem findLinkDefs body

findLinkDefs :: Analysis (Writer [(String,String)])
findLinkDefs (Elem "link_def" _ [Child (Elem "link" _ [(Text key)]),
                                 Child (Elem "url" _ [(Text url)])])
    = tell [(key,url)]
findLinkDefs _ = return ()

fixLinks :: Map String String -> Transform Identity
fixLinks defs (Elem "link" attrs contents) =
  let (newContents, url) =
          case reverse contents of
            -- TODO: non-exhaustive pattern
            Child (Elem "key" _ [Text key]) : rcontents ->
                (reverse rcontents, Map.findWithDefault err key defs)
                    where err = error $ "undefined link key: " ++ key
            _ -> (contents, Map.findWithDefault txt txt defs)
                where txt = stripContents contents
  in return [childElem "a" (("href",url):attrs) newContents]

fixLinks defs (Elem "link_def" _ _) = return []
fixLinks _ x = return [Child x]

-- Turns Markup into Text, stripping all tags.
stripElem (Elem _ _ cs) = stripContents cs
stripContent (Text s) = s
stripContent (Child e) = stripElem e
stripContents = concatMap stripContent
