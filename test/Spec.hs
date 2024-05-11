{-# LANGUAGE OverloadedStrings #-}

import qualified Crapto as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec

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

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Test.Example"
      [ ("prop_reverse", prop_reverse)
      , ("prop_decrapt", prop_decrapt)
      ]

main :: IO ()
main = do
  _results <- sequence [tests]
  hspec $ do
    describe "Crapto" $ do
      it "Should increment each letter alphabetically acording to the fibonacci sequence" $ do
        let (_, _, secret) = C.encrapt "apl"
        secret `shouldBe` "bqn"
      it "Should reverse the operation" $ do
        let (_, _, secret) = C.decrapt "bqn"
        secret `shouldBe` "apl"
