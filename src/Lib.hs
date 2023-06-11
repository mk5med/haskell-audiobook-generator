module Lib
  ( processBook,
  )
where

import Text.Regex.TDFA

-- Checks if a line is a chapter
isChapter :: String -> Bool
isChapter = (=~ "^Chapter +[0-9]+")

-- Checks if a line is a chapter and returns the line if it is
chapterOrEmpty :: String -> String
chapterOrEmpty inStr
  | isChapter inStr = inStr
  | otherwise = ""

-- Removes the chapter line from the first line if it exists
removeChapter :: [String] -> [String]
removeChapter inStrs
  | isChapter (head inStrs) = tail inStrs
  | otherwise = inStrs

-- Split a list of lines into sections separated by chapter
extractSections :: [String] -> [[String]]
extractSections [""] = [[""]]
extractSections [] = [[""]]
extractSections inStrs = do
  -- Remove the chapter if it exists
  let title = chapterOrEmpty (head inStrs)
  let (chapter, next) = break isChapter (removeChapter inStrs)
  -- Add the title back to the chapter text
  (title : chapter) : extractSections next

processBook :: [String] -> [IO ()]
processBook bookLines = do
  -- Extract the sections
  let sections = extractSections bookLines

  -- Index the sections
  let indexedSections = zip sections [0 ..]

  -- Add START and END marks to each section
  let indexedSectionsWithMarks =
        map
          ( \(text :: [String], indx :: Integer) ->
              ( ["START OF CHAPTER " ++ show indx] ++ text ++ ["END OF CHAPTER " ++ show indx],
                indx
              )
          )
          indexedSections
  
  -- Return a list of actions to save the chapters to the file system
  map
    ( \(text :: [String], indx :: Integer) -> do
        writeFile ("chapter_" ++ show indx ++ ".txt") (unlines text)
    )
    indexedSectionsWithMarks
