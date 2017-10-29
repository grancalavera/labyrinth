module Test.Labyrinth
    ( Players
    , Game
    , prop_associativity
    , prop_leftIdentity
    , prop_rightIdentity
    ) where

import           Test.QuickCheck
import           Control.Monad      (replicateM)
import           Data.List          (intercalate)
import           Data.Monoid        ((<>))
import           Labyrinth.Players  (Player(..), Color(..), Players, Name)
import qualified Labyrinth.Players  as Players
import           Labyrinth.Game     (Game(..))

instance Arbitrary Players where
  arbitrary = do
    players <- genPlayers
    return $ foldl (<>) mempty players

instance Arbitrary Game where
  arbitrary = do
    ps <- arbitrary
    p  <- genChoosePlayer ps
    return Game
      { _currentPlayer = p
      , _players = ps
      }

prop_leftIdentity :: (Monoid a, Eq a) => a -> Bool
prop_leftIdentity p = p <> mempty == p

prop_rightIdentity :: (Monoid a, Eq a) => a -> Bool
prop_rightIdentity p = mempty <> p == p

prop_associativity :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_associativity x y z = (x <> y) <> z == x <> (y <> z)

genChoosePlayer :: Players -> Gen (Maybe Player)
genChoosePlayer ps = do
  c <- genColor
  return (Players.lookupByColor c ps)

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
  c <- genColor
  n <- genName
  return Player
    { _color = c
    , _name = n
    }

genPlayers :: Gen [Players]
genPlayers = do
  i <- choose (1, 16)
  replicateM i (Players.fromPlayer <$> genPlayer)
