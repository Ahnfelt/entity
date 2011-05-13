module AsciiShooter.Entity.Tank where

import Feature
import AsciiShooter.Utilities.Mechanics
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Health as Health
import qualified AsciiShooter.Feature.Controller as Controller
import qualified AsciiShooter.Feature.PrimaryWeapon as PrimaryWeapon
import qualified AsciiShooter.Feature.Inventory as Inventory
import qualified AsciiShooter.Feature.Trigger as Trigger

import Data.Typeable
import Data.Record.Label
import Data.Maybe
import Control.Monad

new :: Position -> Velocity -> Game (Entity ())
new position velocity = object $ \this -> do
    physics <- Physics.new position velocity zero (3, 3)
    health <- Health.new 100
    controller <- Controller.new (method control this)
    trigger <- Trigger.new (method trig this)
    primaryWeapon <- PrimaryWeapon.new trigger
    inventory <- Inventory.new []
    return $ toEntity $ controller .:. physics .:. health .:. primaryWeapon .:. inventory .:. nil
    where
        control this = do
            return ()
        trig this = do
            return ()

