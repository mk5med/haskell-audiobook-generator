module Main (main) where

import ArgParse (Flag (..), cmdOptions)
import Lib (processBook)
import System.Environment

-- This is wrapped in a Maybe because arguments are not guaranteed to exist in the flags list
getInput :: [Flag] -> Maybe Flag
getInput [] = Nothing
-- The current element is of type Input
getInput (Input flag : flags) = Just $ Input flag
-- The current element is anything else
getInput (_ : flags) = getInput flags

main :: IO ()
main = do
  -- Extract the filename from the arguments
  -- `file1` is the first argument passed into the program
  _args <- getArgs
  (cmdFlags, _) <- cmdOptions _args

  -- Create a temporary variable `input` and use it to select the next step
  let input = getInput cmdFlags
   in case input of
        Just (Input inputFile) -> process inputFile
        Nothing -> do
          print "Input is a required field"

process inputFile = do
  -- Read the file as a string, split the string by newline
  -- Each string in the list does not have the line terminator character
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/Prelude.html#v:lines
  file <- lines <$> readFile inputFile

  -- processBook returns a list of IO actions to create new files
  -- Iterate and apply each IO action in order
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Base.html#v:sequence
  sequence_ $ processBook file

  putStrLn $ "Finished processing " ++ inputFile