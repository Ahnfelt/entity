module AsciiShooter.Entity.Tank (new) where

import Feature
import AsciiShooter.Key
import AsciiShooter.World
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Feature.Physics (Hit (..))
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Listener as Listener

import Control.Monad

new :: Player -> Position -> Velocity -> Game (Entity ())
new player position velocity = object $ \this -> do
    physics <- Physics.new position velocity zero (3, 3)
    keyListener <- Listener.new (method (onKey player) this)
    hitListener <- Listener.new (method onHit this)
    return $ toEntity $ keyListener .:. physics .:. hitListener .:. nil

onHit this Hit { receiversFault = myFault, hitEntity = entity } = do
    when myFault $ unspawn this

onKey player' this (player, key) = when (player == player') $ case key of
    KeyNorth -> setVelocity (0, 15)
    KeySouth -> setVelocity (0, -15)
    KeyEast -> setVelocity (15, 0)
    KeyWest -> setVelocity (-15, 0)
    KeyBreak -> setVelocity (0, 0)
    KeyFire -> return ()
    where
        setVelocity velocity = do
            let physics = requireFeature this
            Physics.modifyVelocity (const velocity) physics

