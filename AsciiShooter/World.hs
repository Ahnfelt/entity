module AsciiShooter.World where

import Feature
import AsciiShooter.Key

data Direction = North | South | East | West deriving (Eq, Ord, Show)
type Player = Int
type PlayerKey = (Player, Key)

data WorldInput = WorldInput {
    inputKeys :: [PlayerKey]
    }

type WorldOutput = GameState
    
data World = World {
    input :: IO WorldInput,
    output :: WorldOutput -> IO ()
    }

