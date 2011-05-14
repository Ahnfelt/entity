module AsciiShooter.Sprite where

import AsciiShooter.Player
import AsciiShooter.Utilities.Mechanics

data Sprite
    = Tank Direction Player
    | Projectile Player
    | Wall Vector

