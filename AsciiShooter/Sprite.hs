module AsciiShooter.Sprite where

import AsciiShooter.Player
import AsciiShooter.Utilities.Mechanics

data CaterpillarState = CaterpillarState1 | CaterpillarState2
nextCaterpillarState CaterpillarState1 = CaterpillarState2
nextCaterpillarState CaterpillarState2 = CaterpillarState1

data Sprite
    = Tank Direction CaterpillarState Player
    | Projectile Player
    | Wall Vector

