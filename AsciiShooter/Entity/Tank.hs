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
import qualified AsciiShooter.Entity.Projectile as Projectile

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this key -> do
    let initialDirection = North
    direction <- Direction.new initialDirection
    physics <- Physics.new position velocity zero (3, 3) key
    keyListener <- Listener.new (method (onKey player) this)
    hitListener <- Listener.new (method onHit this)
    animation <- Animation.new physics (Tank initialDirection CaterpillarState1 player)
    return $ toEntity $ keyListener .:. direction .:. physics .:. hitListener .:. animation .:. nil

onHit this Hit { receiversFault = myFault, hitEntity = entity } = do
    when (not myFault) $ unspawn this

onKey :: (Has Physics.Type e, Has Animation.Type e) 
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
        void (spawn projectile)
    where
        setDirectionVelocity direction velocity = do
            set Direction.direction direction (requireFeature this)
            Physics.modifyVelocity (const velocity) (requireFeature this)
            -- set Animation.sprite (Tank direction player) (requireFeature this)
            update Animation.sprite (\(Tank _ s _) -> Tank direction s player) (requireFeature this)
        setVelocity velocity = do
            Physics.modifyVelocity (const velocity) (requireFeature this)

