{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Controller where

import Data.Typeable
import Data.Record.Label
import Feature
import qualified AsciiShooter.Feature.Position as Position
import qualified AsciiShooter.Feature.Inventory as Inventory

data Type = Type {
    _controller :: Game ()
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ getL controller self

new :: Game () -> Game Type
new controller = 
    return Type .$. controller

