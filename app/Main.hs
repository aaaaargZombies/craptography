module Main (main) where

import qualified Crapto as C
import Data.Function ((&))
import GHC.IO.Handle (isEOF)
import System.Environment (getArgs, getProgName)


data Opts = Opts
    { inFile :: Maybe String
    , outFile :: Maybe String
    , decrapt :: Bool
    , help :: Bool
    }
    deriving (Show)


defaultOpts :: Opts
defaultOpts = Opts {help = False, decrapt = False, inFile = Nothing, outFile = Nothing}


main :: IO ()
main = do
    args <- getArgs
    let
        opts = parseArguments args defaultOpts
        rotate = if opts & decrapt then C.decrapt else C.encrapt
    if opts & help
        then printHelpText ""
        else case opts & inFile of
            Nothing ->
                ( case opts & outFile of
                    Nothing -> do
                        let
                            (fs, _) = rotate ""
                        stdInOut fs
                    Just outputFile -> do
                        let
                            (fs, msg) = rotate ""
                        writeFile outputFile msg
                        stdInWriteOut outputFile fs
                )
            Just inputFile ->
                ( case opts & outFile of
                    Nothing -> do
                        fileContent <- readFile inputFile
                        let
                            (_, msg) = rotate fileContent
                            msgs = lines msg
                        mapM_ putStrLn msgs
                    Just outputFile -> do
                        fileContent <- readFile inputFile
                        let
                            (_, msg) = rotate fileContent
                        writeFile outputFile msg
                )


parseArguments :: [String] -> Opts -> Opts
parseArguments [] opts = opts
parseArguments (x : xs) opts = parseArguments xs $ updateOpts x xs opts
    where
        updateOpts :: String -> [String] -> Opts -> Opts
        updateOpts x xs opts =
            case x of
                "-h" -> opts {help = True}
                "--help" -> opts {help = True}
                "-i" ->
                    case xs of
                        [] -> opts
                        (f : _) -> opts {inFile = Just f}
                "--input" ->
                    case xs of
                        [] -> opts
                        (f : _) -> opts {inFile = Just f}
                "-o" ->
                    case xs of
                        [] -> opts
                        (f : _) -> opts {outFile = Just f}
                "--output" ->
                    case xs of
                        [] -> opts
                        (f : _) -> opts {outFile = Just f}
                "-d" -> opts {decrapt = True}
                "--decrapt" -> opts {decrapt = True}
                _ -> opts


stdInOut
    :: C.FibState
    -> IO ()
stdInOut fs = do
    end <- isEOF
    if end
        then return ()
        else do
            line <- getLine
            let
                (fs', msg) = C.contRotFib fs line
            putStrLn msg
            stdInOut fs'


stdInWriteOut
    :: FilePath
    -> C.FibState
    -> IO ()
stdInWriteOut fp fs = do
    end <- isEOF
    if end
        then return ()
        else do
            line <- getLine
            let
                (fs', msg') = C.contRotFib fs line
            appendFile fp $ msg' <> "\n"
            stdInWriteOut fp fs'


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
    putStrLn ("   $ cat diaryEntry.txt | " ++ progName ++ " -o secretMessage.txt")
    putStrLn ("   $ " ++ progName ++ " -d -i secretMessage.txt")
    putStrLn "\n"
