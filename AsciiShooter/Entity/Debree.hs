module AsciiShooter.Entity.Debree (new) where

import Feature
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Animation as Animation
import qualified AsciiShooter.Feature.Clock as Clock

import Control.Monad

new :: Position -> Velocity -> Double -> Double -> Game (Entity ())
new position velocity friction time = object $ \this key -> do
    physics <- Physics.new position velocity zero (1, 1) False False key
    animation <- Animation.new physics Debree
    clock <- Clock.new (method (onTick friction time) this)
    return $ toEntity $ physics .:. animation .:. clock .:. nil


onTick friction timeout this currentTime age deltaTime = do
    Physics.modifyVelocity (.* exp (-friction * deltaTime)) (requireFeature this)
    when (age >= timeout) $ unspawn this

