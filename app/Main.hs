module Main (main) where

import qualified Crapto as C
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
--
-- might be a good time to add some tests to start comparing outputs
-- of the different modes to make sure they remain consistent
--
-- I also need to figure out if I'm going to be able to split and
-- restart the decrapt function if I work line by line or if I need
-- to wait for them all to come in then <> and alter that string
-- Don't really like the idea of that, I'd like to be able to do it
-- in stream
--
-- Progress on this now at https://github.com/aaaaargZombies/craptography/blob/2-std//src/Crapto.hs?plan=1#L41-L72
-- Example usage in tests at https://github.com/aaaaargZombies/craptography/blob/2-std//test/Spec.hs?plan=1#L11-L17

main :: IO ()
main = do
  args <- getArgs
  let opts = parseArguments args defaultOpts
      rotate = if opts & decrapt then C.decrapt else C.encrapt
  if opts & help
    then printHelpText ""
    else case opts & inFile of
      Nothing ->
        ( case opts & outFile of
            Nothing -> do
              let (fs, _) = rotate ""
              stdInOut fs
            Just outputFile -> do
              let (fs, msg) = rotate ""
              writeFile outputFile msg
              stdInWriteOut outputFile fs
        )
      Just inputFile ->
        ( case opts & outFile of
            Nothing -> do
              fileContent <- readFile inputFile
              let (_, msg) = rotate fileContent
                  msgs = lines msg
              mapM_ putStrLn msgs
            Just outputFile -> do
              fileContent <- readFile inputFile
              let (_, msg) = rotate fileContent
              writeFile outputFile msg
        )

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn "\n"
  putStrLn msg
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
  putStrLn ("   $ " ++ progName ++ " -i diaryEntry.txt -o secretMessage.txt")
  putStrLn ("   $ " ++ progName ++ " -d -i secretMessage.txt -o message.txt")
  putStrLn "\n"

-- there's a discrepancy here where the `\n` char is added outside of the contRotFib function
-- so it doesn't progress state line in a file line in.
-- This is ok as a basic implementatiion but maybe I need to either figure out how to increment
-- it here or skip it when running through a single string.
stdInOut ::
  C.FibState ->
  IO ()
stdInOut fs = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      let (fs', msg) = C.contRotFib fs line
      putStrLn msg
      stdInOut fs'

stdInWriteOut ::
  FilePath ->
  C.FibState ->
  IO ()
stdInWriteOut fp fs = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      let (fs', msg') = C.contRotFib fs line
      appendFile fp $ msg' <> "\n"
      stdInWriteOut fp fs'
