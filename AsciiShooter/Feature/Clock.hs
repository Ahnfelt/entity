{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables #-}

module AsciiShooter.Feature.Clock (new) where

import Feature

import Data.Typeable
import Data.Record.Label

data Type = Type {
    _originTime :: Double,
    _onTick :: Double -> Double -> Double -> Game ()
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ do
        t <- currentTime
        dt <- deltaTime
        (getL onTick self) t (t - getL originTime self) dt

new :: (Double -> Double -> Double -> Game ()) -> Game Type
new onTick = do
    t <- currentTime
    return Type .$. t .$. onTick


