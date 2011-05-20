{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

module Position where

import Game
import Data.Typeable


type Position = (Double, Double)
type Velocity = (Double, Double)
(.+.) :: (Double, Double) -> (Double, Double) -> (Double, Double)
(.+.) = undefined


data Type = Type {
    position :: Var Position,
    velocity :: Velocity
    } deriving Typeable

new :: Has Type l => Eventually (Entity l) -> Position -> Velocity -> Game (Maybe (Game ()), Type)
new this position velocity = do
    position' <- variable position
    return (Just (method update this), Type {
        position = position',
        velocity = velocity
        })

update :: Has Type l => Entity l -> Game ()
update this = do
    let self = feature this
    modify (.+. velocity self) (position self)


