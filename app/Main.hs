module Main (main) where

import Lib
import System.Environment
import Lib (processBook)

main :: IO ()
main = do
  (file1:_) <- getArgs
  file <- lines <$> readFile file1
  sequence $ processBook file
  putStrLn $ "Finished processing " ++ file1

