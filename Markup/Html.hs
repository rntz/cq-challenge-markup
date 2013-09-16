module Markup.Html ( module Markup.AST
                   , markupToHtml, showHtml, renderMarkupAsHtml )
where

import Text.Html

import Markup.AST

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
markupToHtml = toHtml
