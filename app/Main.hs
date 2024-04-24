module Main (main) where

import Crapto

main :: IO ()
main = interactiveLines

interactiveLines :: IO ()
interactiveLines = do
  line <- getLine
  putStrLn (rotFib line)
  interactiveLines
