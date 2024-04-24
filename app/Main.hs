module Main (main) where

import Crapto
import System.Environment

-- data Arg a
--   = Help
--   | Decrapt
--   | InFile String
--   | OutFile String

data Opts = Opts
  { inFile :: Maybe String
  , outFile :: Maybe String
  , decrapt :: Bool
  , help :: Bool
  }
  deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts{help = False, decrapt = False, inFile = Nothing, outFile = Nothing}

-- I probably want to fold the list of args and update the defaultOpts
parseArguments :: [String] -> Opts
parseArguments args
  | null args = defaultOpts
  | "-h" `elem` args || "--help" `elem` args = Opts{help = False, decrapt = False, inFile = Nothing, outFile = Nothing}
  | otherwise = Opts{help = False, decrapt = False, inFile = Nothing, outFile = Nothing}

main :: IO ()
main = do
  args <- getArgs -- can use this to decide if I stdin or file
  if "-h" `elem` args then printHelpText "" else interactiveLines

interactiveLines :: IO ()
interactiveLines = do
  line <- getLine
  putStrLn (rotFib line)
  interactiveLines

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn "\n"
  progName <- getProgName
  putStrLn (" Usage: " ++ progName ++ " <options>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "   --decapt | -d        - Runs the encraption in reverse"
  putStrLn "   --input  | -i        - Path to file you would like to encrapt"
  putStrLn "            :             if not specified reads from stdin"
  putStrLn "   --output | -o        - Path to file you would like to the write result"
  putStrLn "            :             if not specified writes to stdout"
  putStrLn "   --help   | -h          - Show this help text"
  putStrLn "\n"
  putStrLn " Examples:"
  putStrLn ("   $ cat diaryEntry.txt | " ++ progName ++ " -o secretMessage.txt")
  putStrLn ("   $ " ++ progName ++ " -d -i secretMessage.txt")
  putStrLn "\n"
