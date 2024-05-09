import qualified Crapto as C
import qualified Data.List as List
import Test.Hspec

quote =
  "No one is bored,\
  \everything is boring\
  \  - Mark Fisher"

main :: IO ()
main = hspec $ do
  describe "Crapto" $ do
    it "Should increment each letter alphabetically acording to the fibonacci sequence" $ do
      let (_, _, secret) = C.encrapt "apl"
      secret `shouldBe` "bqn"

    it "Should reverse the operation" $ do
      let (_, _, secret) = C.decrapt "bqn"
      secret `shouldBe` "apl"

    it "multiline text can be handled the same as lists of text using contRotFib" $ do
      let (_, _, secret) = C.encrapt quote
          (x : xs) = lines quote
          -- not sure I trust this because where are the new lines???
          (_, _, secret2) = List.foldr (\line (l, r, acc) -> let (a, b, secret) = C.contRotFib l r line in (a, b, acc <> secret)) (C.encrapt x) xs
      secret `shouldBe` secret2
