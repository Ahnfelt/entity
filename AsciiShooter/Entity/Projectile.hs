module AsciiShooter.Entity.Projectile (new) where

import Feature
import AsciiShooter.Player
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Feature.Physics (Hit (..))
import qualified AsciiShooter.Feature.Direction as Direction
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Listener as Listener
import qualified AsciiShooter.Feature.Animation as Animation
import qualified AsciiShooter.Feature.Damage as Damage
import qualified AsciiShooter.Feature.Random as Random
import qualified AsciiShooter.Entity.Debree as Debree

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this key -> do
    physics <- Physics.new position velocity zero (0.5, 0.5) False True key
    hitListener <- Listener.new (method onHit this)
    animation <- Animation.new physics (Projectile player)
    random <- Random.new
    damage <- Damage.new 10
    return $ toEntity $ physics .:. hitListener .:. animation .:. damage .:. random .:. nil

onHit this Hit { hitEntity = entity } = case getFeature entity of
    Just physics | Physics.getCanBlock physics || Physics.getCanBeBlocked physics -> do
        let random = requireFeature this
        position <- Physics.getPosition (requireFeature this)
        replicateM 10 $ do
            a <- Random.uniform (0, 2 * pi) random
            m <- Random.uniform (0, 20) random
            t <- Random.uniform (0.2, 1) random
            debree <- Debree.new position (vector a m) 5 t
            spawn debree
        unspawn this
    _ -> return ()

