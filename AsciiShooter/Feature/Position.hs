{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Position where

import Data.Typeable
import Data.Record.Label
import Feature
import AsciiShooter.World.Mechanics

data Type = Type {
    _position :: Var Position
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Position -> Game Type
new position = 
    return Type .$. position

moveBy :: Position -> Type -> Game ()
moveBy delta self = update position (.+ delta) self 

moveTo :: Position -> Type -> Game ()
moveTo target self = set position target self 

