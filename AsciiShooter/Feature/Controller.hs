{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Controller where

import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _controller :: Game ()
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ getL controller self

new :: Game () -> Game Type
new controller = 
    return Type .$. controller

