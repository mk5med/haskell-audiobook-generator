module Main (main) where

import ArgParse (Flag (..), FlagEnum (..), cmdOptions, flagListIsValid, flagType)
import AudioConvert (tts, ttsMacOS)
import Lib (ChapterProcessor, extractSections, processBook, splitByLineCount)
import System.Environment

main :: IO ()
main = do
  -- Extract the filename from the arguments
  _args <- getArgs
  (cmdFlags, _) <- cmdOptions _args

  (if flagListIsValid cmdFlags then prepareArgs cmdFlags else showError "REQ_ARGS")

prepareArgs :: [Flag] -> IO ()
prepareArgs cmdFlags = do
  let input = head $ filter (\x -> flagType x == ENUMInput) cmdFlags
  let output = head $ filter (\x -> flagType x == ENUMOutput) cmdFlags

  -- Get the split processor
  -- This will:
  -- 1. Filter the list for split arguments
  -- 2. Take the first split argument
  -- 3. Process it to be a ChapterProcessor
  --
  -- If there are no split arguments then filter returns an empty list, map does nothing, and headOrNothing returns Nothing
  -- The missing patterns in the map are safe because filter only returns Split elements
  let splitProcessor = headOrNothing $ map (\(Split s) -> splitByLineCount s) $ filter (\x -> flagType x == ENUMSplit) cmdFlags
  let processor = processorOrDefault extractSections splitProcessor
  process input output processor

process :: Flag -> Flag -> ChapterProcessor -> IO ()
process (Input inputFile) (Output outputDirectory) chapterProcessor = do
  -- Read the file as a string, split the string by newline
  -- Each string in the list does not have the line terminator character
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/Prelude.html#v:lines
  file <- lines <$> readFile inputFile

  -- processBook returns a list of indexes and associated lines for each index
  -- This converts each group of lines into a single string and creates an IO action
  -- to save to the file system
  let processedBook = processBook file chapterProcessor
  mapM_
    ( \(text, indx) ->
        -- `unlines text` combines the list of strings into a single string with newline delimiters
        writeFile (outputDirectory ++ "chapter_" ++ show indx ++ ".txt") (unlines text)
    )
    processedBook
  mapM_
    ( \(text, indx) -> do
        putStrLn $ "Processing " ++ show indx
        ttsMacOS (unlines text) (outputDirectory ++ "chapter_" ++ show indx)
    )
    processedBook
  putStrLn $ "Finished processing " ++ inputFile
process _ _ _ = showError "REQ_ARGS"

-- Used to fallback to a processor when the user did not select one
processorOrDefault :: ChapterProcessor -> Maybe ChapterProcessor -> ChapterProcessor
processorOrDefault defaultProcessor Nothing = defaultProcessor
processorOrDefault _ (Just processor) = processor

-- Helper method to safely get the first element of lists
headOrNothing :: [a] -> Maybe a
headOrNothing l
  | null l = Nothing
  | otherwise = Just $ head l

-- Helper to print standard error messages
showError :: String -> IO ()
showError "REQ_ARGS" = error "ERROR: Invalid arguments. Expected Input (-i) and Output (-o) arguments"
showError _ = error "Unknown error code"
