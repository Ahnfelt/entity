{-# LANGUAGE FlexibleContexts #-}

module AsciiShooter.Entity.Tank  where

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
import qualified AsciiShooter.Feature.Health as Health
import qualified AsciiShooter.Feature.Random as Random
import qualified AsciiShooter.Entity.Projectile as Projectile
import qualified AsciiShooter.Entity.Debree as Debree

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this key -> do
    let initialDirection = North
    health <- Health.new 20
    direction <- Direction.new initialDirection
    physics <- Physics.new position velocity zero (3, 3) True True key
    keyListener <- Listener.new (method (onKey player) this)
    hitListener <- Listener.new (method onHit this)
    animation <- Animation.new physics (Tank initialDirection CaterpillarState1 player)
    random <- Random.new
    return $ toEntity $ 
        keyListener .:. 
        direction .:. 
        physics .:. 
        hitListener .:. 
        health .:. 
        animation .:. 
        random .:. 
        nil

onHit this Hit { receiversFault = myFault, hitEntity = entity } = unspawn this do
    case getFeature entity of
        Just damage -> do
            h <- Damage.doDamage (requireFeature this) damage
            when (h <= 0) $ do
                let random = requireFeature this
                position <- Physics.getPosition (requireFeature this)
                replicateM 20 $ do
                    x <- Random.uniform (-1, 1) random
                    y <- Random.uniform (-1, 1) random
                    t <- Random.uniform (0.5, 1) random
                    debree <- Debree.new position (x * 40, y * 40) 4 t
                    spawn debree
                unspawn this
        Nothing -> return ()

onKey :: (Has Direction.Type e, Has Physics.Type e, Has Animation.Type e) 
    => Player -> Entity e -> (Player, Key) -> Game () 
onKey player this (player', key) = when (player == player') $ case key of
    KeyNorth -> setDirectionVelocity North (0, 15)
    KeySouth -> setDirectionVelocity South (0, -15)
    KeyEast -> setDirectionVelocity East (15, 0)
    KeyWest -> setDirectionVelocity West (-15, 0)
    KeyBreak -> setVelocity (0, 0)
    KeyFire -> do
        position <- Physics.getPosition (requireFeature this)
        direction <- get Direction.direction (requireFeature this)
        let vector = directionVector direction .* 2
        projectile <- Projectile.new player (position .+. vector .* 2) (vector .* 30)
        spawn projectile
        return ()
    where
        setDirectionVelocity direction velocity = do
            set Direction.direction direction (requireFeature this)
            Physics.modifyVelocity (const velocity) (requireFeature this)
            update Animation.sprite (\(Tank _ s _) -> Tank direction s player) (requireFeature this)
        setVelocity velocity = do
            Physics.modifyVelocity (const velocity) (requireFeature this)

