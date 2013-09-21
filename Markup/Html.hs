module Markup.Html ( module Markup.AST
                   , ToHtmlConfig(..), defaultToHtmlConfig
                   , docToHtml, elemToHtml, contentToHtml, contentsToHtml
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

data ToHtmlConfig = ToHtmlConfig { escapeHtml :: Bool }
defaultToHtmlConfig = ToHtmlConfig { escapeHtml = True }

docToHtml :: ToHtmlConfig -> Doc -> Html
elemToHtml :: ToHtmlConfig -> Elem -> Html
contentToHtml :: ToHtmlConfig -> Content -> Html
contentsToHtml :: ToHtmlConfig -> [Content] -> Html

docToHtml cfg (Doc cs) = contentsToHtml cfg cs
contentsToHtml cfg = concatHtml . map (contentToHtml cfg)

elemToHtml cfg (Elem tagname attrs content) =
    tag tagname ! htmlAttrs attrs $ contentsToHtml cfg content

contentToHtml cfg (Child e) = elemToHtml cfg e
contentToHtml cfg (Text s)
    | escapeHtml cfg = toHtml s
    | otherwise = primHtml s
