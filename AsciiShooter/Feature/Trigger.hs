{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Trigger where

import Data.Typeable
import Data.Record.Label
import Feature
import qualified AsciiShooter.Feature.Magazine as Magazine
import qualified AsciiShooter.Feature.Cooldown as Cooldown

data Type = Type {
    _trigger :: Game ()
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type

new :: Game () -> Game Type
new trigger = 
    return Type .$. trigger

