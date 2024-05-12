{-# LANGUAGE OverloadedStrings #-}

import qualified Crapto as C
import qualified Data.List as List
import Test.Hspec

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_decrapt :: Property
prop_decrapt =
  property $ do
    xs <- forAll $ Gen.string (Range.linear 0 100) Gen.alpha
    let (_, secret) = C.encrapt xs
        (_, plain) = C.decrapt secret
    xs === plain

prop_continue :: Property
prop_continue =
  property $ do
    start <- forAll $ Gen.string (Range.linear 1 10000) Gen.alpha
    middle <- forAll $ Gen.string (Range.linear 1 3) (Gen.choice [pure '\n'])
    end <- forAll $ Gen.string (Range.linear 1 10000) Gen.alpha
    let
      xs = start <> middle <> end
      (_, single) = C.encrapt xs
      lxs = lines xs
      (_, plural) = C.encraptLines lxs
    single === plural

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Test.Crapto"
      [ ("prop_decrapt", prop_decrapt)
      , ("prop_continue", prop_continue)
      ]

main :: IO ()
main = do
  _results <- sequence [tests]
  _recheck <- recheckAt (Seed 3043689854464086461 6114734931481463403) "2:bCeF" prop_continue
  hspec $ do
    describe "Crapto" $ do
      it "Should increment each letter alphabetically acording to the fibonacci sequence" $ do
        let (_, secret) = C.encrapt "apl"
        secret `shouldBe` "bqn"
      it "Should treat a text with new lines the same as a list of text" $ do
        let tstStr = "apl\nThis is a test\nshort\nA much longer line that SHOULD be at the end"
            tstLines = lines tstStr
            (_, secretStr) = C.encrapt tstStr
            (_, secretLines) = C.encraptLines tstLines
        secretLines `shouldBe` secretStr
