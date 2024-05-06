module Crapto (contRotFib, encrapt, decrapt) where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

{-
 Describes the starting Char and length of the set
-}
type Alphabet = (Char, Int)

lowerAlphabet :: Alphabet
lowerAlphabet = ('a', 26)

upperAlphabet :: Alphabet
upperAlphabet = ('A', 26)

digits :: Alphabet
digits = ('0', 10)

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot (startChar, size) n ch = Char.chr $ Char.ord startChar + ((Char.ord ch - Char.ord startChar + n) `mod` size)

digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

rotChar :: Int -> Char -> Char
rotChar n ch
  | Char.isLower ch = lowerRot n ch
  | Char.isUpper ch = upperRot n ch
  | Char.isDigit ch = digitRot n ch
  | otherwise = ch

contRotFib :: Int -> Int -> String -> (Int, Int, String)
contRotFib a b [] = (a, b, "")
contRotFib a b msg = (r, l + r, Text.unpack secret)
 where
  (h, t) = Maybe.fromMaybe ('a', Text.empty) $ Text.uncons $ Text.pack msg
  left = a
  right = b
  (l, r, secret) =
    Text.foldl
      ( \(l, r, acc) char ->
          let n = l + r
              c = rotChar n char
           in (r, n, Text.snoc acc c)
      )
      (left, right, Text.singleton $ rotChar b h)
      t

encrapt :: String -> (Int, Int, String)
encrapt = contRotFib 0 1

decrapt :: String -> (Int, Int, String)
decrapt = contRotFib 0 (-1)
