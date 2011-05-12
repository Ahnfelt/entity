module AsciiShooter.Entity where

data Key = KeyDirection Direction | KeyBreak | KeyFire deriving (Eq, Ord, Show)
type PlayerKey = (Key, Int)

data WorldInput = WorldInput {
    keyPress :: Maybe PlayerKey
    }

data WorldOutput = WorldOutput {
    entityStates :: [EntityState]
    }

