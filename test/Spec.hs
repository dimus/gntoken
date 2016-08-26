import SpecHelper

main :: IO ()
main = hspec $ do
  let text = unlines ["\"Hey,    there it is--",
                      "strangely a semi-reasonable Polar-",
                      "Bear is a specia-",
                      "lity of mine!"]
      ts = tokenize zeroToken text
      hey = head ts
      there = ts !! 1
      is = ts !! 3
      strangely = ts !! 4
      semi = ts !! 6
      polar = ts !! 7
      bear = ts !! 8
      speciality = ts !! 11

  describe "slice" $ do

    it "gets token's substring from a string" $ do
      slice text polar `shouldBe` "Polar-"

  describe "tokenize" $ do

    it "splits a string into tokens" $ do
      length ts `shouldBe` 14

    it "merges words separated by lines in text" $ do
      cleaned speciality `shouldBe` "speciality"

    it "does not merge when next line is capitallized" $ do
      cleaned bear `shouldBe` "Bear"
      cleaned polar `shouldBe` "Polar"

    it "does not merge when dash  stands before non-letter" $ do
      cleaned is `shouldBe` "is"

  describe "verbatim" $ do

    it "returns unmodified string" $ do
      verbatim polar `shouldBe` "Polar-"

    it "drops empty spaces while merging splitted string" $ do
      verbatim speciality `shouldBe` "specia-lity"

  describe "cleaned" $ do

    it "removes non-alphanumeric letters" $ do
      cleaned hey `shouldBe` "Hey"
      cleaned speciality `shouldBe` "speciality"
      cleaned is `shouldBe` "is"

  describe "start" $ do

    it "returns start offset" $ do
      start is `shouldBe` 18
      slice text is `shouldBe` "is--"

  describe "end" $ do

    it "returns end offset" $ do
      end speciality `shouldBe` 80
      slice text speciality `shouldBe` "specia-\nlity"
