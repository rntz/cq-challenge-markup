module Markup.Program (runProgram) where

import Control.Applicative

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Markup.AST
import Markup.Parse

errmsg x = hPutStrLn stderr x
failWith x = do errmsg x; exitFailure

runProgram :: Config -> (Doc -> IO String) -> IO ()
runProgram config render = do
  args <- getArgs
  (srcname, contents) <-
      case args of
        [filename] -> (,) filename <$> readFile filename
        [] -> (,) "<stdin>" <$> getContents
        _ -> failWith "could not parse command-line arguments"
  case parse config srcname contents of
    Left err -> failWith $ show err
    Right markup -> putStr =<< render markup
