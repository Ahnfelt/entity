module AsciiShooter.Entity.Projectile (new) where

import Feature
import AsciiShooter.Key
import AsciiShooter.Player
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Feature.Physics (Hit (..))
import qualified AsciiShooter.Feature.Direction as Direction
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Listener as Listener
import qualified AsciiShooter.Feature.Animation as Animation
import qualified AsciiShooter.Feature.Damage as Damage

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this key -> do
    physics <- Physics.new position velocity zero (1, 1) False True key
    hitListener <- Listener.new (method onHit this)
    animation <- Animation.new physics (Projectile player)
    damage <- Damage.new 10
    return $ toEntity $ physics .:. hitListener .:. animation .:. damage .:. nil

onHit this Hit {} = unspawn this

