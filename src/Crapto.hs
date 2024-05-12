module Crapto (contRotFib, encrapt, decrapt) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Monoid (First)

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

data FibPos
  = First
  | Second
  | Cont
  deriving (Eq, Show)

type FibState = (Int, Int, FibPos)

contRotFib :: FibState -> String -> (FibState, String)
contRotFib fs [] = (fs, "")
contRotFib fst msg = (fs, List.reverse secret)
 where
  (fs, secret) =
    List.foldl
      ( \((l, r, fs), acc) char ->
          case fs of
            First ->
              -- fib 1 = 1
              let c = rotChar r char
               in ((l, r, Second), c : acc)
            Second ->
              -- fib 2 = 1
              let c = rotChar r char
               in ((l, r, Cont), c : acc)
            Cont ->
              -- fib n = (fib n -1) + (fib n -2)
              let n = l + r
                  c = rotChar n char
               in ((r, n, Cont), c : acc)
      )
      (fst, "")
      msg

encrapt :: String -> (FibState, String)
encrapt = contRotFib (1, 1, First) -- I guess this is where I'd need a dependant type to ensure it's valid

decrapt :: String -> (FibState, String)
decrapt = contRotFib (1, -1, First)
