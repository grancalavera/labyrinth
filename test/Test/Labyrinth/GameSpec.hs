module Test.Labyrinth.GameSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.Labyrinth     as Test
import           Test.Labyrinth     (Game)  -- because we add an `Arbitrary`
                                            -- instance there

spec :: Spec
spec = do
  describe "monoid layws" $ do
    it "left identity" $ property (Test.prop_leftIdentity . idg)
    it "right identity" $ property (Test.prop_rightIdentity . idg)
    it "associativity" $ property (Test.prop_associativity . idg)

idg :: Game -> Game
idg = id
