module Markup.Transforms
where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Markup.AST
import Markup.Transform

classA x = ("class", x)
idA x = ("id", x)
hrefA x = ("href", x)

footnote :: Doc -> Doc
footnote doc = putNotes notes doc'
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
