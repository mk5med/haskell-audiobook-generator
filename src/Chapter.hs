module Chapter (
  isChapter, chapterOrEmpty, removeChapter
) where

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