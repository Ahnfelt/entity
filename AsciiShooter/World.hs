module AsciiShooter.World where

import Feature

data Direction = North | South | East | West deriving (Eq, Ord, Show)
data Key = KeyDirection Direction | KeyBreak | KeyFire deriving (Eq, Ord, Show)
type PlayerKey = (Key, Int)

data WorldInput = WorldInput {
    inputKeys :: Maybe PlayerKey
    }

type WorldOutput = GameState
    
data World = World {
    input :: IO WorldInput,
    output :: WorldOutput -> IO ()
    }

