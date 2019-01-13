module Game.TreasureSpec where
import           Test.Hspec
import           Data.Maybe                     ( isJust )
import qualified Labyrinth.Game.Treasure       as T
import           Labyrinth.Game.Treasure        ( Treasure(..) )

spec :: Spec
spec = describe "Treasure" $ do

  let theList = [TA, TB, TC]
      theMap  = T.mapFromList theList

  context "mapFromList" $ do
    it "creates TreasureMap with no Treasure found" $ do
      let actual = T.foundList theMap
      actual `shouldBe` []

    it "creates TreasureMap where all Treasure need to be serched" $ do
      let actual = T.searchingList theMap
      actual `shouldBe` theList

  context "find" $ do
    it "should not find a Treasure that is not in the map" $ do
      let actual = T.find TX theMap
      actual `shouldBe` Nothing

    it "should find a treasure that is in the map and has not been found" $ do
      let result = T.find TA theMap
          actual = isJust result
      actual `shouldBe` True

    it "should not find a treasure that has been already found" $ do
      let actual = T.find TA theMap >>= T.find TA
      actual `shouldBe` Nothing
