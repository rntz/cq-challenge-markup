module Main where

import Control.Applicative

import Markup.AST
import Markup.Html
import Markup.Parse
import Markup.Program (runProgram)
import Markup.Sexp
import Markup.XML
import qualified Markup.Transforms as T

parseConfig :: ParseConfig
parseConfig = defaultParseConfig
              { isSubdocumentTag = \x -> elem x ["note"]
              , parseLinks = True }

toHtmlConfig :: ToHtmlConfig
toHtmlConfig = defaultToHtmlConfig

renderMarkup :: Elem -> String
--renderMarkup = showXML . elemToXML
--renderMarkup = showSexp . elemToSexp True -- True meaning "with attrs"
renderMarkup = showHtml . elemToHtml toHtmlConfig

program :: Doc -> IO String
program d = renderMarkup . docToElem . T.footnotes . T.links
            . T.numberHeadings T.defaultNumberHeadingsConfig
            <$> T.includes parseConfig d

main :: IO ()
main = runProgram parseConfig program
