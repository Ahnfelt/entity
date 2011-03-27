{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.PrimaryWeapon where

import Data.Typeable
import Data.Record.Label
import Feature
import qualified AsciiShooter.Feature.Trigger as Trigger

data Type = Type {
    _weapon :: Var Trigger.Type 
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Trigger.Type -> Game Type
new trigger = 
    return Type .$. trigger

