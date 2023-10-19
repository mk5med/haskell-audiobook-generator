module AudioConvert (tts, ttsMacOS) where
import System.Process

tts :: String -> String -> IO String
tts inStr fileName = readProcess "espeak" [inStr, "-w", fileName] ""

ttsMacOS :: String -> String -> IO String
ttsMacOS inStr fileName = readProcess "say" [inStr, "-o", fileName] ""