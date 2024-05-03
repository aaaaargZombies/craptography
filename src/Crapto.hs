module Crapto (encrapt, decrapt) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

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

rotFib :: Int -> Int -> String -> String
rotFib _ _ [] = ""
rotFib a b msg = Text.unpack secret
 where
  (h, t) = Maybe.fromMaybe ('a', Text.empty) $ Text.uncons $ Text.pack msg
  left = a
  right = b
  (_, _, secret) =
    Text.foldl
      ( \(l, r, acc) char ->
          let n = l + r
              c = rotChar n char
           in (r, n, Text.snoc acc c)
      )
      (left, right, Text.singleton $ rotChar b h)
      t

encrapt :: String -> String
encrapt = rotFib 0 1

decrapt :: String -> String
decrapt = rotFib 0 (-1)
