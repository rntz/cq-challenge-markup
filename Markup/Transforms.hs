module Markup.Transforms (includes, links, footnotes)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import System.Exit (exitFailure)
import System.IO

import Markup.AST
import Markup.Parse
import Markup.Transform

classA x = ("class", x)
idA x = ("id", x)
hrefA x = ("href", x)


-- Including other markup documents via \include.
includes :: Config -> Doc -> IO Doc
includes cfg = transformDoc (include cfg)

include :: Config -> Transform IO
include cfg (Elem "include" _ [Text path]) = do
  contents <- readFile path
  case parse cfg path contents of
    Right (Doc content) -> return content
    Left err -> do hPutStrLn stderr $ show err
                   exitFailure
include _ e = return [Child e]


-- Parsing links & link defs into HTML anchors.
links :: Doc -> Doc
links doc = runIdentity $ transformDoc (fixLinks defs) doc
    where defs = Map.fromList $ execWriter $ analyzeDoc findLinkDefs doc

findLinkDefs :: Analysis (Writer [(String,String)])
findLinkDefs (Elem "link_def" _ [Child (Elem "link" _ [(Text key)]),
                                 Child (Elem "url" _ [(Text url)])])
    = tell [(key,url)]
findLinkDefs _ = return ()

fixLinks :: Map String String -> Transform Identity
fixLinks defs (Elem "link" attrs contents) =
  let (newContents, url) =
          case reverse contents of
            -- TODO: non-exhaustive pattern
            Child (Elem "key" _ [Text key]) : rcontents ->
                (reverse rcontents, Map.findWithDefault err key defs)
                    where err = error $ "undefined link key: " ++ key
            _ -> (contents, Map.findWithDefault txt txt defs)
                where txt = stripContents contents
  in return [childElem "a" (("href",url):attrs) newContents]

fixLinks defs (Elem "link_def" _ _) = return []
fixLinks _ x = return [Child x]

-- Turns Markup into Text, stripping all tags.
stripElem (Elem _ _ cs) = stripContents cs
stripContent (Text s) = s
stripContent (Child e) = stripElem e
stripContents = concatMap stripContent


-- Lifting footnotes out of the body
footnotes :: Doc -> Doc
footnotes doc = putNotes notes doc'
    where (doc', notes) = runState (transformDoc getNotes doc) []

-- gets a list of notes in reverse order, and replaces \note{}s with links to
-- #note${n}.
getNotes :: Transform (State [(Attrs,[Content])])
getNotes (Elem "note" attrs content) = do
  modify ((attrs,content):)
  n <- show . length <$> get
  let url = "#note-" ++ n
      text = "[" ++ n ++ "]"
      id = "noteref-" ++ n
  return [childElem "a" [hrefA url, classA "noteref", idA id]
                        [childElem "sup" [] [Text text]]]
getNotes e = return [Child e]

-- Puts a list of notes (in reverse order!) at the end of the document.
putNotes :: [(Attrs,[Content])] -> Doc -> Doc
putNotes rnotes (Doc contents) = Doc $ contents ++ [noteSection]
    where
      noteSection = childElem "section" [("class", "footnotes")] notes
      notes = zipWith notify [1..] (reverse rnotes)
      -- should this be a div or a section?

notify :: Int -> (Attrs,[Content]) -> Content
notify noteId (attrs_, content_) = childElem "div" attrs content
    where
      n = show noteId
      -- TODO: the anchor really needs to go inside the first paragraph in
      -- the note. But what if it's not a paragraph? :(
      attrs = classA "footnote" : idA ("note-" ++ n) : attrs_
      anchor = childElem "a" [hrefA ("#noteref-" ++ n)] [Text text]
      text = "[" ++ n ++ "]"
      -- If the footnote starts with a paragraph (which it probably does), we
      -- insert the anchor at the beginning of the paragraph, with a space after
      -- it. Otherwise we insert it before all the content of the footnote.
      content = case content_ of
                  Child (Elem "p" pattrs pcontent) : rest ->
                      childElem "p" pattrs (anchor : Text " " : pcontent) : rest
                  _ -> anchor : content_
