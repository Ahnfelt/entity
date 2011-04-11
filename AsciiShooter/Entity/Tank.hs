module AsciiShooter.Entity.Tank where

import Data.Typeable
import Data.Record.Label
import Data.Maybe
import Control.Monad
import Feature
import qualified AsciiShooter.Feature.Position as Position
import qualified AsciiShooter.Feature.Health as Health
import qualified AsciiShooter.Feature.Controller as Controller
import qualified AsciiShooter.Feature.PrimaryWeapon as PrimaryWeapon
import qualified AsciiShooter.Feature.Inventory as Inventory
import qualified AsciiShooter.Feature.Trigger as Trigger

new :: Game (Entity ())
new = object $ \this -> do
    position <- Position.new (0, 0)
    health <- Health.new 100
    controller <- Controller.new (method control this)
    trigger <- Trigger.new (method trig this)
    primaryWeapon <- PrimaryWeapon.new trigger
    inventory <- Inventory.new []
    return $ toEntity $ controller .:. position .:. health .:. primaryWeapon .:. inventory .:. nil
    where
        control this = do
            let position = requireFeature this
            (x, y) <- get Position.position position
            if x >= 10 
                then Position.moveTo (0, y) position
                else Position.moveBy (1, 0) position
            let weapon = requireFeature this
            when (x == 0) $ do
                trigger <- get PrimaryWeapon.weapon weapon
                getL Trigger.trigger trigger
        trig this = do
            return ()

