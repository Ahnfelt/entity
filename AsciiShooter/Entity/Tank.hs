module AsciiShooter.Entity.Tank where

import Data.Typeable
import Data.Record.Label
import Data.Maybe
import Control.Monad
import Feature
import qualified AsciiShooter.Feature.Position as Position
import qualified AsciiShooter.Feature.Velocity as Velocity
import qualified AsciiShooter.Feature.Health as Health
import qualified AsciiShooter.Feature.Controller as Controller
import qualified AsciiShooter.Feature.PrimaryWeapon as PrimaryWeapon
import qualified AsciiShooter.Feature.Inventory as Inventory
import qualified AsciiShooter.Feature.Trigger as Trigger

new :: Game (Entity ())
new = object $ \this -> do
    position <- Position.new (10, 10)
    velocity <- Velocity.new (0, 1) position
    health <- Health.new 100
    controller <- Controller.new (method control this)
    trigger <- Trigger.new (method trig this)
    primaryWeapon <- PrimaryWeapon.new trigger
    inventory <- Inventory.new []
    return $ toEntity $ controller .:. position .:. velocity .:. health .:. primaryWeapon .:. inventory .:. nil
    where
        control this = do
            let position = requireFeature this
            (x, y) <- get Position.position position
            when (x >= 20) $ Position.moveTo (0, y) position
            let weapon = requireFeature this
            when (x == 0) $ do
                trigger <- get PrimaryWeapon.weapon weapon
                getL Trigger.trigger trigger
        trig this = do
            return ()

