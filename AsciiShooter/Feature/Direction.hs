{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Direction where

import Data.Typeable
import Data.Record.Label
import Feature
import AsciiShooter.Utilities.Mechanics

data Type = Type {
    _direction :: Var Direction
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Direction -> Game Type
new direction = 
    return Type .$. direction

