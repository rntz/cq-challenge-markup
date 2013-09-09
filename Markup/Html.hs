module Markup.Html ( module Markup.AST
                   , markupToHtml, contentToHtml, showMarkupAsHtml )
where

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Html

import Markup.AST

-- TODO: parse links, link-defs, etc.
instance HTML Elem where
    toHtml = markupToHtml

-- TODO: toHtml for Strings assumes that the content-encoding of the page is
-- going to be utf8. is there some way to fix this?
instance HTML Content where
    toHtml (Text s) = toHtml s
    toHtml (Child e) = toHtml e

contentToHtml :: Content -> Html
contentToHtml = toHtml

-- renderHTML doesn't fuck up whitespace inside of tags where it matters,
-- although it does produce HTML that's ugly as hell.
showMarkupAsHtml :: Elem -> String
showMarkupAsHtml = renderHtml

markupToHtml :: Elem -> Html
markupToHtml body = htmlElem (parseLinkDefs body) body

htmlElem :: Map String String -> Elem -> Html
htmlElem defs (Elem "link" xs) =
    case reverse xs of
      [Text txt] -> anchor![href url] $ toHtml txt
          where url = Map.findWithDefault txt txt defs
      Child (Elem "key" [Text key]) : rxs ->
          case Map.lookup key defs of
            Just url -> anchor![href url] $ concatHtml $
                        map (htmlContent defs) $ reverse rxs
            Nothing -> error $ "undefined link key: " ++ key
      _ -> error "invalid link"
htmlElem defs (Elem "link_def" _) = noHtml
htmlElem defs (Elem tagname contents)
    -- FIXME: remove "True || "
    | True || elem (map toUpper tagname) validHtmlTags =
        tag tagname $ concatHtml $ map toHtml contents
    | otherwise = error $ "invalid tag: " ++ tagname

htmlContent defs (Text s) = toHtml s
htmlContent defs (Child e) = htmlElem defs e

parseLinkDefs :: Elem -> Map String String
parseLinkDefs (Elem "link_def" xs) =
    case xs of
      [Child (Elem "link" [Text key]), Child (Elem "url" [Text url])] ->
          Map.singleton key url
      _ -> error "invalid link_def"
parseLinkDefs (Elem _ xs) = Map.unions (map contentLinkDefs xs)
    where contentLinkDefs (Text _) = Map.empty
          contentLinkDefs (Child e) = parseLinkDefs e
