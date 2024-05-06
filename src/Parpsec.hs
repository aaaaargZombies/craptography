module Parpsec where

import Data.Char
import Text.Parsec

-- Parse a single character
parseChar :: Parsec String () Char
parseChar = anyChar

-- Transform a character according to the specifications
transformChar :: Int -> Char -> Char
transformChar n c
  | isLower c = chr $ ord 'a' + ((ord c - ord 'a' + n) `mod` 26)
  | isUpper c = chr $ ord 'A' + ((ord c - ord 'A' + n) `mod` 26)
  | isDigit c = chr $ ord '0' + ((ord c - ord '0' + n) `mod` 10)
  | otherwise = c -- Ignore non-letter, non-digit characters

-- Parse and transform a character
parseAndTransform :: Int -> Parsec String () Char
parseAndTransform n = do
  transformChar n <$> parseChar

-- Parse and transform text
-- probably need to fo my scan thing here some how
-- maybe `manyAccum` would be better???
parseAndTransformText :: Int -> Parsec String () String
parseAndTransformText n = many (parseAndTransform n)

-- Example usage:
-- main :: IO ()
-- main = do
--   let input = "abcXYZ123"
--   case parse (parseAndTransformText 3) "" input of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right result -> putStrLn result -- Output: "defABC456"
