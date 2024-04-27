module Main (main) where

import Control.Exception
import Crapto
import Data.Function ((&))
import GHC.IO.Handle (isEOF)
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

-- TODO
--
-- I also need to decide how I'm going to encode/decode from std in
-- currently I restart on each line so there's a diference between
-- readinf from a file and stdin
--
-- I'm continuing to increment fib sequence on non Alphabet words
-- so will need to watch for that in the difference as we'll loose
-- the '\n' on stdin

main :: IO ()
main = do
  args <- getArgs
  let opts = parseArguments args defaultOpts
      rotate = if opts & decrapt then unRotFib else rotFib
  case opts & inFile of
    Nothing ->
      ( case opts & outFile of
          Nothing -> interactiveLines rotate
          Just o -> do
            end <- isEOF
            if end
              then return ()
              else do
                line <- getLine
                writeFile o (rotate line)
                interactiveLinesAppend rotate o
      )
    Just i ->
      ( case opts & outFile of
          Nothing -> do
            fileContent <- readFile i
            putStrLn (rotate fileContent)
          Just o -> do
            fileContent <- readFile i
            writeFile o (rotate fileContent)
      )

interactiveLines :: (String -> String) -> IO ()
interactiveLines cipher = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      putStrLn $ cipher line
      interactiveLines cipher

interactiveLinesAppend :: (String -> String) -> String -> IO ()
interactiveLinesAppend cipher outputFile = do
  end <- isEOF
  if end
    then appendFile outputFile ""
    else do
      line <- getLine
      appendFile outputFile $ "\n" <> cipher line
      interactiveLinesAppend cipher outputFile

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn "\n"
  progName <- getProgName
  putStrLn (" Usage: " ++ progName ++ " <options>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "   --decrapt | -d        - Runs the encraption in reverse"
  putStrLn "   --input   | -i        - Path to file you would like to encrapt"
  putStrLn "             :             if not specified reads from stdin"
  putStrLn "   --output  | -o        - Path to file you would like to the write result"
  putStrLn "             :             if not specified writes to stdout"
  putStrLn "   --help    | -h        - Show this help text"
  putStrLn "\n"
  putStrLn " Examples:"
  putStrLn ("   $ cat diaryEntry.txt | " ++ progName ++ " -o secretMessage.txt")
  putStrLn ("   $ " ++ progName ++ " -d -i secretMessage.txt")
  putStrLn "\n"
