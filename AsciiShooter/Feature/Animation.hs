{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Animation where

import Feature
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Sprite
import qualified AsciiShooter.Feature.Physics as Physics

import Data.Typeable
import Data.Record.Label

data Type = Type {
    _physics :: Physics.Type,
    _sprite :: Var Sprite
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ do
        update sprite animate self

new :: Physics.Type -> Sprite -> Game Type
new physics sprite = 
    return Type .$. physics .$. sprite

setSprite :: Sprite -> Type -> Game ()
setSprite sprite' self = set sprite sprite' self

getPositionedSprite :: Type -> Game (Position, Sprite)
getPositionedSprite self = do
    p <- Physics.getPosition (getL physics self)
    s <- get sprite self
    return (p, s)

animate (Tank direction caterpiller player) = Tank direction (nextCaterpillarState caterpiller) player -- TODO animate based on drived distance!
animate sprite = sprite
