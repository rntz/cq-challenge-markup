module Markup.Html ( module Markup.AST
                   , markupToHtml, showMarkupAsHtml )
where

import Control.Monad.Identity
import Control.Monad.Writer

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Html

import Markup.AST
import Markup.Translate

-- renderHTML doesn't fuck up whitespace inside of tags where it matters,
-- although it does produce HTML that's ugly as hell.
showMarkupAsHtml :: Elem -> String
showMarkupAsHtml = renderHtml

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
    where newBody = runIdentity $ translateElem (fixLinks defs) body
          defs = Map.fromList $ execWriter $ analyzeElem findLinkDefs body

findLinkDefs :: Analysis (Writer [(String,String)])
findLinkDefs (Child (Elem "link_def" _ [Child (Elem "link" _ [(Text key)]),
                                        Child (Elem "url" _ [(Text url)])]))
    = tell [(key,url)]
findLinkDefs _ = return ()

fixLinks :: Map String String -> Translation Identity
fixLinks defs (Child (Elem "link" attrs contents)) =
  let (newContents, url) =
          case reverse contents of
            [Text txt] -> (contents, Map.findWithDefault txt txt defs)
            Child (Elem "key" _ [Text key]) : rcontents ->
                (reverse rcontents, Map.findWithDefault err key defs)
                    where err = error $ "undefined link key: " ++ key
  in return [Child (Elem "a" (("href",url):attrs) newContents)]

fixLinks defs (Child (Elem "link_def" _ _)) = return []
fixLinks _ x = return [x]
