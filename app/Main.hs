module Main (main) where

import Crapto
import System.Environment

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
parseArguments :: [String] -> Opts -> Opts
parseArguments [] opts = opts
parseArguments (x : xs) opts = parseArguments xs $ updateOpts x xs opts
 where
  updateOpts :: String -> [String] -> Opts -> Opts
  updateOpts x xs opts =
    case x of
      "-h" -> opts{help = True}
      "--help" -> opts{help = True}
      "-i" ->
        case xs of
          [] -> opts
          (f : _) -> opts{inFile = Just f}
      "--input" ->
        case xs of
          [] -> opts
          (f : _) -> opts{inFile = Just f}
      "-o" ->
        case xs of
          [] -> opts
          (f : _) -> opts{outFile = Just f}
      "--output" ->
        case xs of
          [] -> opts
          (f : _) -> opts{outFile = Just f}
      "-d" -> opts{decrapt = True}
      "--decrapt" -> opts{decrapt = True}
      _ -> opts

main :: IO ()
main = do
  args <- getArgs
  interactiveLines $ parseArguments args defaultOpts

interactiveLines :: Opts -> IO ()
interactiveLines opts = do
  line <- getLine
  if decrapt opts
    then putStrLn (unRotFib line)
    else putStrLn (rotFib line)
  interactiveLines opts

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn "\n"
  progName <- getProgName
  putStrLn (" Usage: " ++ progName ++ " <options>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "   --decrapt | -d        - Runs the encraption in reverse"
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
