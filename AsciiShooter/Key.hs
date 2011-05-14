{-# LANGUAGE DeriveDataTypeable #-}

module AsciiShooter.Key where

import Data.Typeable

data Key 
    = KeyNorth 
    | KeySouth 
    | KeyWest 
    | KeyEast 
    | KeyBreak
    | KeyFire
    deriving (Typeable, Show, Eq)

