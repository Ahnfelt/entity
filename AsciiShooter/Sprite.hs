module AsciiShooter.Sprite where

import AsciiShooter.Player
import AsciiShooter.Utilities.Mechanics

data Direction = North | South | East | West deriving (Eq, Ord, Show)

data CaterpillarState = CaterpillarState1 | CaterpillarState2
nextCaterpillarTracksState CaterpillarState1 = CaterpillarState2
nextCaterpillarTracksState CaterpillarState2 = CaterpillarState1

data Sprite
    = Tank Direction CaterpillarState Player
    | Projectile Player
    | Wall Vector

