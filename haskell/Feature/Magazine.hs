{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Feature.Magazine where

import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _magazines :: Var Int,
    _capacity :: Int,
    _ammo :: Var Int
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Int -> Int -> Int -> Game Type
new magazines capacity ammo = 
    return Type .$. magazines .$. capacity .$. ammo

