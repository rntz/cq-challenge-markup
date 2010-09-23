module Main where

import System.IO

import Text.Markup.AST
import Text.Markup.Parse
import Text.Markup.XML

config :: Config
config = defaultConfig { isSubdocumentTag = flip elem ["note"] }

main :: IO ()
main = do input <- getContents
          let result = parse config "<stdin>" input
          case result of
            Right markup -> putStrLn $ showMarkupAsXML markup
            Left error -> hPutStrLn stderr $ show error
