module Main where

import Control.Applicative

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Markup.AST
import Markup.Parse
import Markup.XML
import Markup.Sexp

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

config :: Config
config = defaultConfig { isSubdocumentTag = \x -> elem x ["note"]
                       , parseLinks = True }

--showMarkup = showMarkupAsXML
showMarkup = showMarkupAsSexp

main :: IO ()
main = do
  args <- getArgs
  (srcname, input) <-
      case args of
        [filename] -> (,) filename <$> readFile filename
        [] -> (,) "<stdin>" <$> getContents
        _ -> failWith "could not parse command-line arguments"
  let result = parse config srcname input
  case result of
    Right markup -> putStrLn $ showMarkup markup
    Left error -> hPutStrLn stderr $ show error
