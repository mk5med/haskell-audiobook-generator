{-
This module implements argument processing from the command line.
Supported arguments:
- s, splitCount. If this is unset the processor will default to splitting by chapter identifier
- i, input file
- o, output directory
-}
module ArgParse (cmdOptions, Flag (..), FlagEnum (..), flagType, flagTypeIsInList, flagListIsValid) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt

data Flag = Split Integer | Input String | Output String deriving (Show)

data FlagEnum = ENUMSplit | ENUMInput | ENUMOutput deriving (Eq)

readAsInt :: String -> Integer
readAsInt a = read a :: Integer

splitCount :: Maybe String -> Flag
-- Read as: (inputStrOrNull) => Split( readAsInt( fromMaybe(default=0, inputStrOrNull) ) )
splitCount = Split . readAsInt . fromMaybe "1000"

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["input"] (ReqArg Input "FILE") "(REQUIRED) Path to input file",
    Option ['o'] ["output"] (ReqArg Output "DIR") "(REQUIRED) Path to output directory",
    Option ['s'] ["split"] (OptArg splitCount "NUMBER") "Split the input file by NUMBER.\nIf omitted the program will use a heuristic chapter processor"
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

{-
 HELPER METHODS FOR ARGUMENT PROCESSING
-}

-- Map flags to an enum to allow filtering and searching by type
flagType :: Flag -> FlagEnum
flagType (Input _) = ENUMInput
flagType (Output _) = ENUMOutput
flagType (Split _) = ENUMSplit

-- Check that a flag type (e.g: Input, Output) is in a list of flags
flagTypeIsInList :: FlagEnum -> [Flag] -> Bool
flagTypeIsInList f = any (\x -> flagType x == f)

-- Check that all REQUIRED arguments are available
flagListIsValid :: [Flag] -> Bool
flagListIsValid l = flagTypeIsInList ENUMInput l && flagTypeIsInList ENUMOutput l