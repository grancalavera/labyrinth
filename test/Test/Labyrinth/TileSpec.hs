module Test.Labyrinth.TileSpec where

import           Test.Hspec
import qualified Labyrinth.Tile as Tile
import           Labyrinth.Tile (Tile(..), Terrain(..), Direction(..))

spec :: Spec
spec = do
  describe "Transforming tiles" $ do

    it "should rotate a tile" $ do
      let original = Tile Path North
          rotated = Tile Path West
      Tile.rotate original `shouldBe` rotated

    it "rotate' is rotate clockwise" $ do
      let original = Tile Path North
          rotated' = Tile Path East
      Tile.rotate' original `shouldBe` rotated'
