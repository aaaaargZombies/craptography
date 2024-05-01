module Crapto where

import qualified Data.Foldable as S
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

indexOf :: (Eq a) => a -> [a] -> Int
indexOf _ [] = undefined
indexOf a (x : xs) = if x == a then 0 else 1 + indexOf a xs

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

rotFib :: String -> String
rotFib [] = ""
rotFib msg = go (left, right, [rotChar 1 h]) t
 where
  left = 0
  right = 1
  (h : t) = msg

  go :: (Int, Int, String) -> String -> String
  go (_, _, acc) [] = acc
  go (l, r, acc) (x : xs) =
    let n = (l + r)
        a = rotChar n x
     in go (r, n, acc <> [a]) xs

unRotFib :: String -> String
unRotFib [] = ""
unRotFib msg = go (left, right, [rotChar (-1) h]) t
 where
  left = 0
  right = -1
  (h : t) = msg

  go :: (Int, Int, String) -> String -> String
  go (_, _, acc) [] = acc
  go (l, r, acc) (x : xs) =
    let n = (l + r)
        a = rotChar n x
     in go (r, n, acc <> [a]) xs
