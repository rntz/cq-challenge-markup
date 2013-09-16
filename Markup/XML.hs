module Markup.XML ( module Markup.AST
                  , docToXML, elemToXML, contentToXML
                  , showXML )
where

import Markup.AST
import qualified Text.XML.Light as XML

docToXML :: Doc -> [XML.Content]
docToXML (Doc cs) = map contentToXML cs

elemToXML :: Elem -> XML.Element
elemToXML e = XML.unode tag contents
    where tag = elemTag e
          contents = map contentToXML $ elemContent e

contentToXML :: Content -> XML.Content
contentToXML (Text s) = XML.Text $ XML.blank_cdata { XML.cdData = s }
contentToXML (Child e) = XML.Elem $ elemToXML e

showXML :: XML.Element -> String
showXML = XML.ppElement
