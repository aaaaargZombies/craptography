{-# LANGUAGE OverloadedStrings #-}

import qualified Crapto as C
import qualified Data.List as List
import Test.Hspec

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

prop_decrapt :: Property
prop_decrapt =
  property $ do
    xs <- forAll $ Gen.string (Range.linear 0 100) Gen.alpha
    let (_, _, secret) = C.encrapt xs
        (_, _, plain) = C.decrapt secret
    xs === plain

prop_continue :: Property
prop_continue =
  property $ do
    xs <- forAll $ Gen.string (Range.linear 0 10000) (Gen.choice [Gen.alpha, pure '\n'])
    let
      (_, _, single) = C.encrapt xs
      (h, ts) = Maybe.fromMaybe (xs, []) $ List.uncons $ lines xs
      (_, _, plural) =
        List.foldr
          (\line (l, r, acc) -> let (a, b, secret) = C.contRotFib l r ("\n" <> line) in (a, b, acc <> secret))
          (C.encrapt h)
          ts
    single === plural

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Test.Example"
      [ ("prop_reverse", prop_reverse)
      , ("prop_decrapt", prop_decrapt)
      , ("prop_continue", prop_continue)
      ]

main :: IO ()
main = do
  _results <- sequence [tests]
  _recheck <- recheckAt (Seed 3043689854464086461 6114734931481463403) "2:bCeF" prop_continue
  hspec $ do
    describe "Crapto" $ do
      it "Should increment each letter alphabetically acording to the fibonacci sequence" $ do
        let (_, _, secret) = C.encrapt "apl"
        secret `shouldBe` "bqn"
