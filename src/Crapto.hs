module Crapto where

import Data.Function ((&))
import qualified Data.List as L
import qualified Data.Sequence as S

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Alphabet = [Char]

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isDigit :: Char -> Bool
isDigit = (`elem` digits)

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

-- The following steps still have to be taken care of:

-- 1. Get the index of a letter within an alphabet
-- 2. Get the letter at a certain index within an alphabet
-- 3. Transform a string, character by character

listLength :: [a] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

indexOf :: (Eq a) => a -> [a] -> Int
indexOf _ [] = undefined
indexOf a (x : xs) = if x == a then 0 else 1 + indexOf a xs

-- indexOf :: Char -> Alphabet -> Int
-- indexOf _ [] = undefined
-- indexOf ch (x:xs) = if x == ch then 0 else 1 + indexOf ch xs

indexOfInt :: Int -> [Int] -> Int
indexOfInt _ [] = undefined
indexOfInt ch (x : xs) = if x == ch then 0 else 1 + indexOfInt ch xs

-- getIndex :: [a] -> Int -> a
-- getIndex [] _ = undefined
-- getIndex (x : xs) i = if i == 0 then x else getIndex xs (i - 1)

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch = alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | isDigit ch = digitRot n ch
  | otherwise = ch

rotFibChar :: Int -> Char -> Char
rotFibChar n = rotChar (fib (n))

caesar :: Int -> Alphabet -> Alphabet
caesar n = map (rotChar n)

rot13 :: String -> String
rot13 = caesar 13

rotFib :: String -> String
rotFib msg =
  msg
    & S.fromList
    & S.mapWithIndex (\n c -> rotFibChar (n + 1) c)
    & show -- this is bad  "fromList \"cqm\""

unRotFib :: String -> String
unRotFib msg =
  msg
    & S.fromList
    & S.mapWithIndex (\n c -> rotFibChar ((-n) - 1) c)
    & show -- this is bad  "fromList \"cqm\""

fib :: Int -> Int
fib n = f n
 where
  f n =
    case compare n 0 of
      LT -> fn n
      EQ -> 0
      GT -> fp n

  fp n
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = fp (n - 1) + fp (n - 2)

  fn n
    | n == (-1) = -1
    | n == (-2) = -1
    | otherwise = fn (n + 1) + fn (n + 2)
