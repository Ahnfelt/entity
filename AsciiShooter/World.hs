module AsciiShooter.World where

import Feature
import AsciiShooter.Player
import AsciiShooter.Key
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics

import Data.Map (Map)

type PlayerKey = (Player, Key)

data WorldInput = WorldInput {
    inputKeys :: [PlayerKey]
    }

data WorldOutput = WorldOutput {
    outputSprites :: Map EntityKey (Position, Sprite)
    }
    
data World = World {
    input :: IO WorldInput,
    output :: WorldOutput -> IO ()
    }

