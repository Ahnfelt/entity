module AsciiShooter.Entity.Wall (new) where

import Feature
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Animation as Animation

import Control.Monad

new :: Box -> Game (Entity ())
new box = object $ \this -> do
    physics <- Physics.new (center box) zero zero (dimensions box)
    animation <- Animation.new physics (Wall (dimensions box))
    return $ toEntity $ physics .:. animation .:. nil

