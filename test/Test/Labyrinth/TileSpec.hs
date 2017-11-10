module Test.Labyrinth.TileSpec where

import           Test.Hspec
import qualified Labyrinth.Tile as Tile
import           Labyrinth.Tile (Edge (..), Terrain (..))

spec :: Spec
spec = do
  describe "Transforming tiles" $ do

    it "should rotate a tile" $ do
      let original  = Tile.make Path [South, North] Tile.origin
          rotated   = Tile.make Path [West, East] Tile.origin
      Tile.rotate original `shouldBe` rotated

    it "a mirrored path is the same path" $ do
      let path  = Tile.make Path [South, North] Tile.origin
      Tile.mirror path `shouldBe` path

    it "should mirror forks" $ do
      let original = Tile.make Fork [North, West, East] Tile.origin
          mirrored = Tile.make Fork [South, East, West] Tile.origin
      Tile.mirror original `shouldBe` mirrored

    it "rotate . mirror is rotate counter clock wise" $ do
      let original    = Tile.make Fork [North, West, East] Tile.origin
          rotatedCCW  = Tile.make Fork [East, North, South] Tile.origin
      (Tile.rotate . Tile.mirror) original `shouldBe` rotatedCCW
