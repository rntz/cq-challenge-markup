module Main where

import Control.Applicative

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Markup.AST
import Markup.Html
import Markup.Parse
import Markup.Sexp
import Markup.XML
import Markup.Program (runProgram)

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

config :: Config
config = defaultConfig { isSubdocumentTag = \x -> elem x ["note"]
                       , parseLinks = True }

--showMarkup = showMarkupAsXML
--showMarkup = showMarkupAsSexp
showMarkup = showMarkupAsHtml

main :: IO ()
main = runProgram config showMarkup
