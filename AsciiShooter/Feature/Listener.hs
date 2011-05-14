{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables #-}

module AsciiShooter.Feature.Listener (Type, new, fireEvent) where

import Feature

import Data.Typeable
import Data.Record.Label

data Type e = Type {
    _listen :: e -> Game (),
    _pending :: Var [e]
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable (Type e) where
    updater self = Just $ do
        let f = getL listen self
        events <- get pending self
        set pending [] self
        mapM_ f (reverse events)

new :: forall e. (e -> Game ()) -> Game (Type e)
new listen = do
    return Type .$. listen .$. ([] :: [e])

fireEvent :: e -> Type e -> Game ()
fireEvent event self = do
    update pending (event :) self

