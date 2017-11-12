module Test.Labyrinth.TileSpec where

import           Test.Hspec
import qualified Data.Set       as Set
import           Lens.Micro     ((^.))
import qualified Labyrinth.Tile as Tile
import           Labyrinth.Tile (Edge (..), Terrain (..), edges)

spec :: Spec
spec = do
  describe "Transforming tiles" $ do

    it "should rotate a tile" $ do
      let original  = Tile.fromTerrain Path
          rotated   = Set.fromList [West, East]
      Tile.rotate original ^. edges `shouldBe` rotated

    it "a mirrored path is the same path" $ do
      let path  = Tile.fromTerrain Path
      Tile.mirror path `shouldBe` path

    it "should mirror forks" $ do
      let original = Tile.fromTerrain Fork
          mirrored = Set.fromList [South, East, West]
      Tile.mirror original ^. edges `shouldBe` mirrored

    it "rotate . mirror is rotate counter clock wise" $ do
      let original    = Tile.fromTerrain Fork
          rotatedCCW  = Set.fromList [East, North, South]
      (Tile.rotate . Tile.mirror) original ^. edges `shouldBe` rotatedCCW

