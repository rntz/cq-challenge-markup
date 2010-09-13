module Main where

import System.IO

import Text.Markup.AST
import Text.Markup.Parse
import Text.Markup.XML

main :: IO ()
main = do input <- getContents
          let result = parse (const False) "<stdin>" input
          case result of
            Right markup -> putStrLn $ showMarkupAsXML markup
            Left error -> hPutStrLn stderr $ show error
