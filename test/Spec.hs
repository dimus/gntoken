import SpecHelper

main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "converts a string into list of tokens" $ do
      let t = head . tokenize $ "hey, Jude, don't make it bad\n" ++
                                 "Take a sad song and make it better"
      verbatim t `shouldBe` "hey"
