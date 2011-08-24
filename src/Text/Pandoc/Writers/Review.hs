{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Review
   Copyright   : Copyright (C) 2011 Masayoshi Takahashi
   License     : GNU GPL, version 2 or above 

   Maintainer  : takahashim@gmail.com
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ReVIEW markup.

ReVIEW:  <http://github.com/kmuto/review>
-}
module Text.Pandoc.Writers.Review ( writeReview ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
-- import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intercalate )
import Control.Monad.State


data WriterState = WriterState {
    stNotes     :: [String]        -- Footnotes
  , stTables    :: [String]        -- Table IDs
--  , stListLevel :: [Char]          -- String at beginning of list items, e.g. "**"
--  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to Review.
writeReview :: WriterOptions -> Pandoc -> String
writeReview opts document =
  evalState (pandocToReview opts document)
            (WriterState { stNotes = [], stTables = [] })

-- | Return Review representation of document.
pandocToReview :: WriterOptions -> Pandoc -> State WriterState String
pandocToReview opts (Pandoc _ blocks) = do
  body <- blockListToReview opts blocks
  notes <- liftM (unlines . reverse . stNotes) get
  let main = body ++ if null notes then "" else ("\n\n" ++ notes)
  let context = writerVariables opts ++ [ ("body", main) ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main


-- | Escape one character as needed for Review.
escapeCharForReview :: Char -> String
escapeCharForReview x = case x of
--                         '&'    -> "&amp;"
--                         '<'    -> "&lt;"
--                         '>'    -> "&gt;"
--                         '"'    -> "&quot;"
--                         '*'    -> "&#42;"
--                         '_'    -> "&#95;"
--                         '@'    -> "&#64;"
--                         '|'    -> "&#124;"
                         c      -> [c]

-- | Escape string as needed for Review.
escapeStringForReview :: String -> String
escapeStringForReview = concatMap escapeCharForReview

-- | Escape one character as needed for Review.
escapeInlineChar :: Char -> String
escapeInlineChar x = case x of
  '}'    -> "\\}"
  c      -> [c]

-- | Escape string in inline-contents for Review.
escapeInlineString :: String -> String
escapeInlineString = concatMap escapeInlineChar

-- | Convert Pandoc block element to Review.
blockToReview :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToReview _ Null = return ""

blockToReview opts (Plain inlines) =
  inlineListToReview opts inlines

blockToReview opts (Para [Image txt (src,tit)]) = do
  capt <- blockToReview opts (Para txt)
  im <- inlineToReview opts (Image txt (src,tit))
  return $ im ++ "\n" ++ capt ++ "\n"

blockToReview opts (Para inlines) = do
  contents <- inlineListToReview opts inlines
  return $ contents ++ "\n"

blockToReview _ (RawBlock f str) =
  if f == "review"
     then return str
     else return ""

blockToReview _ HorizontalRule = return "//hr\n"

blockToReview opts (Header level inlines) = do
  contents <- inlineListToReview opts inlines
  let prefix = replicate level '='
  return $ "\n" ++ prefix ++ " " ++ contents ++ "\n"

blockToReview _ (CodeBlock (_,_,_) str) =
  return $ "//emlist{\n" ++ str ++ "\n//}\n"
--    where classes' = if null classes
--                        then ""
--                        else "(" ++ unwords classes ++ ")"

blockToReview opts (BlockQuote bs@[Para _]) = do
  contents <- blockListToReview opts bs
  return $ "//quote{\n" ++ contents ++ "\n//}\n"

blockToReview opts (BlockQuote blocks) = do
  contents <- blockListToReview opts blocks
  return $ "//quote{\n" ++ contents ++ "\n//}\n"

blockToReview opts (Table capt aligns widths headers rows') = do
  curTables <- liftM stTables get
  let newnum = length curTables + 1
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return "[]"
                   else do
                      c <- inlineListToReview opts capt
                      return $ "[" ++ c ++ "]"
  let percent w = show (truncate (100*w) :: Integer)
  let coltags = if all (== 0.0) widths
                   then ""
                   else "//tsize[" ++ intercalate "," (map percent widths) ++ "]\n"
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToReview opts alignStrings 0 headers
                 return $ hs ++ "\n--------------------\n"
  body' <- zipWithM (tableRowToReview opts alignStrings) [1..] rows'
  let thistable = "tbl" ++ show newnum
  modify $ \s -> s { stTables = thistable : curTables }
  return $ coltags ++ "//table[" ++ thistable ++ "]" ++ captionDoc ++ "{\n" ++ head' ++ unlines body' ++ "//}\n"

blockToReview opts (BulletList items) = do
  contents <- mapM (bulletListItemToReview opts) items
  return $ vcat contents ++ "\n"

blockToReview opts (OrderedList _ items) = do
  contents <- mapM (orderedListItemToReview opts) items
  return $ vcat contents ++ "\n"

blockToReview opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToReview opts) items
  return $ vcat contents ++ "\n"

-- Auxiliary functions for lists:

-- | Convert bullet (not ordered) list item (list of blocks) to Review.
bulletListItemToReview :: WriterOptions -> [Block] -> State WriterState String
bulletListItemToReview opts items = do
  contents <- blockListToReview opts items
  return $ marker ++ " " ++ contents
  where marker = " * "

-- | Convert ordered (not bullet) list item (list of blocks) to Review.
orderedListItemToReview :: WriterOptions -> [Block] -> State WriterState String
orderedListItemToReview opts items = do
  contents <- blockListToReview opts items
  return $ marker ++ " " ++ contents
  where marker = " 1. " -- too easy, but not invalid...

-- | Convert definition list item (label, list of blocks) to Review.
definitionListItemToReview :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToReview opts (label, items) = do
  labelText <- inlineListToReview opts label
  contents <- mapM (blockListToReview opts) items
  return $ ": " ++ labelText ++ "\n" ++
          "\t" ++ (intercalate "\n\t" contents) ++ "\n"

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables. (TODO: these are common to HTML, MediaWiki,
-- and Review writers, and should be abstracted out.)

tableRowToReview :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToReview opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToReview opts celltype alignment item)
            alignStrings cols'
  return $ intercalate "\t" cols''

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToReview :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToReview opts _ _ item = do
  let mkcell x = if x == "" then "." else x
  contents <- blockListToReview opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Review.
blockListToReview :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToReview opts blocks =
  mapM (blockToReview opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to Review.
inlineListToReview :: WriterOptions -> [Inline] -> State WriterState String
inlineListToReview opts lst =
  mapM (inlineToReview opts) lst >>= return . concat

inlineMarkup :: String -> String -> String
inlineMarkup command contents = "@<" ++ command ++ ">{" ++ (escapeInlineString contents) ++ "}"

-- | Convert Pandoc inline element to Review.
inlineToReview :: WriterOptions -> Inline -> State WriterState String

inlineToReview opts (Emph lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "b" contents

inlineToReview opts (Strong lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "strong" contents

inlineToReview opts (Strikeout lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "del" contents

inlineToReview opts (Superscript lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "sup" contents

inlineToReview opts (Subscript lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "sub" contents

inlineToReview opts (SmallCaps lst) = inlineListToReview opts lst

inlineToReview opts (Quoted SingleQuote lst) = do
  contents <- inlineListToReview opts lst
  return $ "'" ++ contents ++ "'"

inlineToReview opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToReview opts lst
  return $ "\"" ++ contents ++ "\""

inlineToReview opts (Cite _  lst) = do
  contents <- inlineListToReview opts lst
  return $ inlineMarkup "cite" contents

inlineToReview _ EmDash = return " -- "

inlineToReview _ EnDash = return " - "

inlineToReview _ Apostrophe = return "'"

inlineToReview _ Ellipses = return "..."

inlineToReview _ (Code _ str) =
  return $ inlineMarkup "tt" str

inlineToReview _ (Str str) = return $ escapeStringForReview str

inlineToReview _ (Math _ str) =
  return $ inlineMarkup "m" str

inlineToReview _ (RawInline f str) =
  if f == "review"
     then return str
     else return ""

inlineToReview _ (LineBreak) = return $ (inlineMarkup "br" "") ++ "\n"

inlineToReview _ Space = return " "

inlineToReview opts (Link txt (src, _)) = do
  label <- case txt of
                [Code _ s]  -> return s
                _           -> inlineListToReview opts txt
  let contents = if src == label
                then src
                else src ++ "," ++ label
  return $ inlineMarkup "href" contents

inlineToReview _ (Image _ (source, tit)) = do
  return $ "//image[" ++ source ++ "][" ++ tit ++ "]{\n//}\n"

inlineToReview opts (Note contents) = do
  curNotes <- liftM stNotes get
  let newnum = length curNotes + 1
  contents' <- blockListToReview opts contents
  let thisnote = "//footnote[fn" ++ show newnum ++ "][" ++ contents' ++ "]\n"
  modify $ \s -> s { stNotes = thisnote : curNotes }
  return $ inlineMarkup "fn" ("fn" ++ show newnum)
  -- note - may not work for notes with multiple blocks
