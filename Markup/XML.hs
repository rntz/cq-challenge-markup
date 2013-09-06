module Markup.XML ( module Markup.AST
                  , markupToXML, contentToXML, showMarkupAsXML )
where

import Markup.AST
import qualified Text.XML.Light as XML

markupToXML :: Elem -> XML.Element
markupToXML e = XML.unode tag contents
    where tag = elemTag e
          contents = map contentToXML $ elemContent e

contentToXML :: Content -> XML.Content
contentToXML (Text s) = XML.Text $ XML.blank_cdata { XML.cdData = s }
contentToXML (Child e) = XML.Elem $ markupToXML e

showMarkupAsXML :: Elem -> String
showMarkupAsXML = XML.ppElement . markupToXML
