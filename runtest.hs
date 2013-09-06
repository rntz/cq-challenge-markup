module Main where

import Control.Monad (when)
import Control.Applicative

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import qualified Text.XML.Light as XML

import Markup.AST
import Markup.Parse
import Markup.XML

config :: Config
config = defaultConfig { isSubdocumentTag = \x -> elem x ["note"] }

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

main :: IO ()
main = do
  -- Parse the input file as markup
  [testname] <- getArgs
  let filename = testname ++ ".txt"
  input <- readFile filename
  ours <- case parse config filename input of
            Right markup -> return markup
            Left error -> failWith $ show error
  let ourXML = markupToXML ours
  -- Parse the corresponding test case
  theirs <- readFile (testname ++ ".xml")
  theirXML <- case XML.parseXMLDoc theirs of
                Nothing -> failWith "could not parse reference XML"
                Just x -> return x

  -- Compare ours & theirs by serializing to strings & checking that they are
  -- equal. This is silly.
  when (XML.showElement ourXML /= XML.showElement theirXML) $
    -- TODO: show a diff?
    failWith "Outputs were not equal!"
