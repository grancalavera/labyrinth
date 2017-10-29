module Test.Labyrinth.TileSpec where

import           Test.Hspec
import qualified Data.Set       as Set
import qualified Labyrinth.Tile as Tile
import           Labyrinth.Tile (Tile (..), Edge (..), Terrain (..))

spec :: Spec
spec = do
  describe "Transforming tiles" $ do

    it "should rotate a tile" $ do
      let original  = Tile Path (Set.fromList [South, North])
          rotated   = Tile Path (Set.fromList [West, East])
      Tile.rotate original `shouldBe` rotated

    it "a mirrored path is the same path" $ do
      let path  = Tile Path (Set.fromList [South, North])
      Tile.mirror path `shouldBe` path

    it "should mirror forks" $ do
      let original = Tile Fork (Set.fromList [North, West, East])
          mirrored = Tile Fork (Set.fromList [South, East, West])
      Tile.mirror original `shouldBe` mirrored

    it "rotate . mirror is rotate counter clock wise" $ do
      let original    = Tile Fork (Set.fromList [North, West, East])
          rotatedCCW  = Tile Fork (Set.fromList [East, North, South])
      (Tile.rotate . Tile.mirror) original `shouldBe` rotatedCCW
