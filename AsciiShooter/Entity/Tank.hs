module AsciiShooter.Entity.Tank (new) where

import Feature
import AsciiShooter.Key
import AsciiShooter.Player
import AsciiShooter.World
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Feature.Physics (Hit (..))
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Listener as Listener
import qualified AsciiShooter.Feature.Animation as Animation

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this -> do
    physics <- Physics.new position velocity zero (3, 3)
    keyListener <- Listener.new (method (onKey player) this)
    hitListener <- Listener.new (method onHit this)
    animation <- Animation.new physics (Tank North player)
    return $ toEntity $ keyListener .:. physics .:. hitListener .:. animation .:. nil

onHit this Hit { receiversFault = myFault, hitEntity = entity } = do
    when myFault $ unspawn this

onKey player this (player', key) = when (player == player') $ case key of
    KeyNorth -> setDirectionVelocity North (0, 15)
    KeySouth -> setDirectionVelocity South (0, -15)
    KeyEast -> setDirectionVelocity East (15, 0)
    KeyWest -> setDirectionVelocity West (-15, 0)
    KeyBreak -> setVelocity (0, 0)
    KeyFire -> unspawn this
    where
        setDirectionVelocity direction velocity = do
            Physics.modifyVelocity (const velocity) (requireFeature this)
            set Animation.sprite (Tank direction player) (requireFeature this)
        setVelocity velocity = do
            Physics.modifyVelocity (const velocity) (requireFeature this)

