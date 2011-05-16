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
        s <- get sprite self
        case s of 
            (Tank direction _ player) -> do
                d <- Physics.getDistanceTraveled (getL physics self)
                let caterpillerState = if even (truncate d) then CaterpillarState1 else CaterpillarState2
                set sprite (Tank direction caterpillerState player) self
            _ -> return ()

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


