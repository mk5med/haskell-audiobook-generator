{-
This module implements argument processing from the command line.
Supported arguments:
- s, splitCount. If this is unset the processor will default to splitting by chapter identifier
- i, input file
- o, output directory
-}
module ArgParse (cmdOptions, Flag (..)) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt

data Flag = Split Int | Input String | Output String deriving (Show)

readAsInt :: String -> Int
readAsInt a = read a :: Int

splitCount :: Maybe String -> Flag
-- (inputStrOrNull) => Split( readAsInt( fromMaybe(default=0, inputStrOrNull) ) )
splitCount = Split . readAsInt . fromMaybe "0"

options :: [OptDescr Flag]
options =
  [ Option ['s'] ["split"] (OptArg splitCount "NUMBER") "Split the input file by NUMBER",
    Option ['i'] ["input"] (ReqArg Input "FILE") "Path to input file",
    Option ['o'] ["output"] (ReqArg Output "DIR") "Path to output directory"
  ]

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/System-Console-GetOpt.html#t:ArgOrder
cmdOptions :: [String] -> IO ([Flag], [String])
cmdOptions argv =
  -- Permute through the options with the arguments from the process
  case getOpt Permute options argv of
    -- Normal case
    (o, n, []) -> return (o, n)
    -- Errors
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo __header__ options))
  where
    __header__ = "Usage: ic [OPTION...] files..."