{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables #-}

module AsciiShooter.Feature.Random (new, uniform) where

import Feature
import Data.Typeable
import Data.Record.Label

import System.Random

data Type = Type {
    _generator :: Var StdGen
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type

new :: Game Type
new = do
    g <- newGenerator
    return Type .$. g

uniform :: Random a => (a, a) -> Type -> Game a
uniform range self = do
    g <- get generator self
    let (a, g') = randomR range g
    set generator g' self
    return a

