module Main where

import Control.Applicative

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Markup.AST
import Markup.Html
import Markup.Parse
import Markup.Program (runProgram)
import Markup.Sexp
import Markup.XML

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

config :: Config
config = defaultConfig { isSubdocumentTag = \x -> elem x ["note"]
                       , parseLinks = True }

--renderMarkup = renderMarkupAsXML
--renderMarkup = renderMarkupAsSexp
renderMarkup = renderMarkupAsHtml

main :: IO ()
main = runProgram config (renderMarkup . docToElem)
