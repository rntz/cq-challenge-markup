module Markup.Html ( module Markup.AST
                   , docToHtml, elemToHtml, contentToHtml
                   , showHtml )
where

import Text.Html

import Markup.AST

-- renderHTML doesn't fuck up whitespace inside of tags where it matters,
-- although it does produce HTML that's ugly as hell.
showHtml :: Html -> String
showHtml = renderHtml

htmlAttrs :: [(String,String)] -> [HtmlAttr]
htmlAttrs = map (uncurry strAttr)

instance HTML Doc where
    toHtml (Doc cs) = toHtmlFromList cs

instance HTML Elem where
    toHtml (Elem tagname attrs children) =
        tag tagname ! map (uncurry strAttr) attrs $ toHtmlFromList children

instance HTML Content where
    -- TODO: toHtml for Strings assumes that the content-encoding of the page is
    -- going to be utf8. is there some way to fix this?
    toHtml (Text s) = toHtml s
    toHtml (Child e) = toHtml e

docToHtml :: Doc -> Html
elemToHtml :: Elem -> Html
contentToHtml :: Content -> Html

docToHtml = toHtml
elemToHtml = toHtml
contentToHtml = toHtml
