module Game.NewGameSpec where
import           Test.Hspec

import           Data.Maybe               (isJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Labyrinth.Game           (Color (..), PlayOrder (..),
                                           Player (..))
import           Labyrinth.Game.Cell      (Terrain (..))
import           Labyrinth.Game.Direction (Direction (..))
import           Labyrinth.Game.NewGame   (HasTreasure, TileD, addPlayers,
                                           addPositions, addTreasures,
                                           availablePositions, chooseDirections)
import qualified Labyrinth.Game.Player    as P
import           Labyrinth.Game.Position  (Position)
import           Labyrinth.Game.Treasure  (Treasure (..))
import           Lens.Micro               ((^.), _3)
import           Linear.V2                (V2 (..))

spec :: Spec
spec = describe "NewGame" $ do
  context "availablePositions" $ do
    let positions =
          Set.fromList
            [ V2 0 0
            , V2 0 1
            , V2 0 2
            , V2 1 0
            , V2 1 1
            , V2 1 2
            , V2 2 0
            , V2 2 1
            , V2 2 2
            ] :: Set Position
        posDescriptions =
          Set.fromList
            [ Just $ V2 0 0
            , Just $ V2 0 1
            , Just $ V2 0 2
            , Nothing
            , Nothing
            , Nothing
            , Just $ V2 2 0
            , Just $ V2 2 1
            , Just $ V2 2 2
            ] :: Set (Maybe Position)

    it "should be calculated from missing possitions" $ do
      let actual = availablePositions positions posDescriptions
      actual `shouldBe` [V2 1 0, V2 1 1, V2 1 2]

  context "addPositions" $ do
    it "should assing missing positions" $ do
      let pos = [V2 0 0] :: [Position]
          tiles =
            [ (Path, Nothing      , Nothing, False, Nothing)
            , (Path, Just (V2 0 1), Nothing, False, Nothing)
            ] :: [TileD HasTreasure PlayOrder]
          expected =
            Just
              [ (Path, Just (V2 0 1), Nothing, False, Nothing)
              , (Path, Just (V2 0 0), Nothing, False, Nothing)
              ] :: Maybe [TileD HasTreasure PlayOrder]
          actual = addPositions pos tiles

      actual `shouldBe` expected

    it "should fail when there are less positions" $ do
      let tiles =
            [ (Path, Nothing      , Nothing, False, Nothing)
            , (Path, Just (V2 0 1), Nothing, False, Nothing)
            ] :: [TileD HasTreasure PlayOrder]
          actual = addPositions [] tiles

      actual `shouldBe` Nothing

    it "should fail when there are more positions" $ do
      let pos = [V2 0 0, V2 1 0] :: [Position]
          tiles =
            [ (Path, Nothing      , Nothing, False, Nothing)
            , (Path, Just (V2 0 1), Nothing, False, Nothing)
            ] :: [TileD HasTreasure PlayOrder]
          actual = addPositions pos tiles

      actual `shouldBe` Nothing

  context "chooseDirections"
    $ it "should choose random directions for missing directions"
    $ do
        let tiles =
              [ (Path, Nothing      , Just North, False, Nothing)
              , (Path, Just (V2 0 1), Nothing   , False, Nothing)
              ] :: [TileD HasTreasure PlayOrder]

        actual <- chooseDirections tiles
        all (isJust . (^. _3)) actual `shouldBe` True

  context "addTreasures" $ do
    it "should add treasures" $ do
      let treasures = [TA, TB]
          tiles =
            [ (Path, Nothing, Nothing, True , Nothing)
            , (Path, Nothing, Nothing, False, Nothing)
            , (Path, Nothing, Nothing, True , Nothing)
            ] :: [TileD HasTreasure PlayOrder]
          expected =
            Just
              [ (Path, Nothing, Nothing, Nothing, Nothing)
              , (Path, Nothing, Nothing, Just TA, Nothing)
              , (Path, Nothing, Nothing, Just TB, Nothing)
              ] :: Maybe [TileD (Maybe Treasure) PlayOrder]
      addTreasures treasures tiles `shouldBe` expected

    it "should fail if there are less treasures" $ do
      let tiles =
            [(Path, Nothing, Nothing, True, Nothing)] :: [ TileD
                  HasTreasure
                  PlayOrder
              ]
      addTreasures [] tiles `shouldBe` Nothing

    it "should fail if there are more treasures" $ do
      let treasures = [TA, TB]
          tiles =
            [(Path, Nothing, Nothing, True, Nothing)] :: [ TileD
                  HasTreasure
                  PlayOrder
              ]
      addTreasures treasures tiles `shouldBe` Nothing

  context "sddPlayers" $ do
    it "should fail if need to add more players than given" $ do
      let players = P.fromList [Player "p1" Yellow First]
          tiles =
            [ (Path, Nothing, Nothing, False, Just First)
            , (Path, Nothing, Nothing, False, Just Second)
            ] :: [TileD HasTreasure PlayOrder]
      addPlayers players tiles `shouldBe` Nothing

    it "should fail if need to add less players than given" $ do
      let tiles =
            [ (Path, Nothing, Nothing, False, Just First)
            , (Path, Nothing, Nothing, False, Just Second)
            ] :: [TileD HasTreasure PlayOrder]
      addPlayers mempty tiles `shouldBe` Nothing

    it "should fail when trying to add same PlayOrder more than once" $ do
      let players =
            P.fromList [Player "p1" Yellow First, Player "p2" Red Second]
          tiles =
            [ (Path, Nothing, Nothing, False, Just First)
            , (Path, Nothing, Nothing, False, Just First)
            ] :: [TileD HasTreasure PlayOrder]
      addPlayers players tiles `shouldBe` Nothing


    it "should add players to the game" $ do
      let p1      = Player "p1" Yellow First
          p2      = Player "p2" Yellow Second
          players = P.fromList [p1, p2]
          tiles =
            [ (Path, Nothing, Nothing, False, Just First)
            , (Path, Nothing, Nothing, False, Just Second)
            , (Path, Nothing, Nothing, False, Nothing)
            ] :: [TileD HasTreasure PlayOrder]
          expected =
            Just
              [ (Path, Nothing, Nothing, False, Nothing)
              , (Path, Nothing, Nothing, False, Just p1)
              , (Path, Nothing, Nothing, False, Just p2)
              ] :: Maybe [TileD HasTreasure Player]

      addPlayers players tiles `shouldBe` expected
