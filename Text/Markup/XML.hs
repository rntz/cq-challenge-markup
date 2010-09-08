module Text.Markup.XML ( module Text.Markup.AST
                       , markupToXML, contentToXML, showMarkupAsXML )
where

import Text.Markup.AST
import qualified Text.XML.Light as XML

markupToXML :: Elem -> XML.Element
markupToXML e = XML.unode tag contents
    where tag = elemTag e
          contents = map contentToXML $ elemContent e

contentToXML :: Content -> XML.Content
contentToXML (Text s) = XML.Text $ XML.blank_cdata { XML.cdData = s }
contentToXML (Child e) = XML.Elem $ markupToXML e

-- If you're not familiar with Haskell, infix `.' is function composition.
-- Writing functions without variables by using function composition and
-- combinators is called "points-free style". Its proper usage is a matter of
-- much debate. I feel this example is simple enough to merit it.
showMarkupAsXML :: Elem -> String
showMarkupAsXML = XML.ppElement . markupToXML
