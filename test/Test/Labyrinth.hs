module Test.Labyrinth (Players) where

import           Test.QuickCheck
import           Labyrinth.Players  (Player(..), Color(..), Players, Name)
import qualified Labyrinth.Players  as Players
import           Control.Monad      (replicateM)
import           Data.List          (intercalate)
import           Data.Monoid        ((<>))

genChar :: Gen Char
genChar = arbitrary

genColor :: Gen Color
genColor = elements [Yellow, Blue, Green, Red]

genNamePart :: Gen Name
genNamePart = do
  i <- choose (1, 10)
  replicateM i genChar

genName :: Gen Name
genName = do
  i     <- choose (1, 4)
  parts <- replicateM i genNamePart
  return (intercalate " "  parts)

genPlayer :: Gen Player
genPlayer = do
  color <- genColor
  name  <- genName
  return (Player color name)

genPlayers :: Gen [Players]
genPlayers = do
  i <- choose (1, 16)
  replicateM i (Players.fromPlayer <$> genPlayer)

instance Arbitrary Players where
  arbitrary = do
    players <- genPlayers
    return $ foldl (<>) mempty players
