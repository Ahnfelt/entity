module AsciiShooter.Sprite where

import AsciiShooter.Player
import AsciiShooter.Utilities.Mechanics

data Direction = North | South | East | West deriving (Eq, Ord, Show)

data Sprite
    = Tank Direction Player
    | Projectile Player
    | Wall Vector

