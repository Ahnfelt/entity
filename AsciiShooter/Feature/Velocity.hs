{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Velocity where

import Data.Typeable
import Data.Record.Label

import Feature
import AsciiShooter.Utilities.Mechanics
import qualified AsciiShooter.Feature.Position as Position

data Type = Type {
    _velocity :: Var Velocity,
    _positionFeature :: Position.Type
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ do
        dt <- deltaTime
        v <- get velocity self
        let p = getL positionFeature self
        Position.moveBy (v .* dt) p

new :: Velocity -> Position.Type -> Game Type
new velocity positionFeature = 
    return Type .$. velocity .$. positionFeature

changeTo :: Velocity -> Type -> Game ()
changeTo velocity' self = set velocity velocity' self 

