module Main (main) where

import System.Environment
import Lib (processBook)

main :: IO ()
main = do
  -- Extract the filename from the arguments
  -- `file1` is the first argument passed into the program
  (file1:_) <- getArgs

  -- Read the file as a string, split the string by newline
  -- Each string in the list does not have the line terminator character
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/Prelude.html#v:lines
  file <- lines <$> readFile file1

  -- processBook returns a list of IO actions to create new files
  -- Iterate and apply each IO action in order
  -- https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Base.html#v:sequence
  _ <- sequence $ processBook file
  
  putStrLn $ "Finished processing " ++ file1

