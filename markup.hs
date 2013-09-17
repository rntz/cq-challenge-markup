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
import qualified Markup.Transforms as T

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

config :: Config
config = defaultConfig { isSubdocumentTag = \x -> elem x ["note"]
                       , parseLinks = True }

renderMarkup :: Elem -> String
--renderMarkup = showXML . elemToXML
--renderMarkup = showSexp . elemToSexp True -- True meaning "with attrs"
renderMarkup = showHtml . elemToHtml

program :: Doc -> IO String
program d = renderMarkup . docToElem . T.footnotes . T.links
            <$> T.includes config d

main :: IO ()
main = runProgram config program
