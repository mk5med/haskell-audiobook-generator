module Lib
  ( processBook,
    substring,
    splitByLineCount,
  )
where

import Chapter (chapterOrEmpty, isChapter, removeChapter)

-- Split a list of lines into sections separated by chapter
extractSections :: [String] -> [[String]]
extractSections [""] = [[""]]
extractSections [] = [[""]]
extractSections inStrs = do
  -- Remove the chapter if it exists
  let title = chapterOrEmpty (head inStrs)

  -- Split the `inStrs` list into two
  -- The first list contains all elements up to the first line that matches the pattern for chapter delimiters
  -- break :: (a -> Bool) -> [a] -> ([a], [a])
  -- Return: (in-order list of elements that do not match, next element that matches + everything else)
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/Prelude.html#v:break
  let (chapterContent, next) = break isChapter (removeChapter inStrs)

  -- Add the title back to the chapter text
  (title : chapterContent) : extractSections next

substring :: Integer -> [String] -> ([String], [String])
-- Case: No available strings
substring _ [] = ([], [])
-- Case: Reached end of substring length
substring 0 s = ([], s)
substring i (cur : _rem) =
  -- Keep taking elements until an end case is reached
  let (y, ys) = substring (i - 1) _rem
   in -- Combines thunks to create: ( cur1:(cur2:( ...:[] )), [] or remainer)
      (cur : y, ys)

splitByLineCount :: Integer -> [String] -> [[String]]
splitByLineCount _ [] = []
splitByLineCount l inStrs =
  -- Get the substring
  let (section, remainder) = substring l inStrs
   in -- Append the substring to the front and process the remainder
      -- Combines thunks to create: section1:(section2: ...:[] )
      section : splitByLineCount l remainder

addMarks :: [String] -> Integer -> [String]
addMarks text indx = ["START OF CHAPTER " ++ show indx] ++ text ++ ["END OF CHAPTER " ++ show indx]

processBook :: [String] -> [IO ()]
processBook bookLines = do
  -- Extract the sections
  let sections = extractSections bookLines

  -- Index the sections
  let indexedSections = zip sections [0 ..]

  -- Add START and END marks to each section
  let indexedSectionsWithMarks =
        map
          ( \(text, indx) -> (addMarks text indx, indx)
          )
          indexedSections

  -- Return a list of actions to save the chapters to the file system
  map
    ( \(text, indx) -> do
        writeFile ("chapter_" ++ show indx ++ ".txt") (unlines text)
    )
    indexedSectionsWithMarks
