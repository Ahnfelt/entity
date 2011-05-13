{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Physics (
    Type, new, 
    modifyAcceleration, modifyVelocity,
    getAcceleration, getVelocity, getPosition,
    getBoundingBox
    ) where

import Feature
import AsciiShooter.Utilities.Mechanics

import Data.Typeable
import Data.Record.Label
import Data.Maybe
import Control.Monad

data Type = Type {
    _position :: Var Position,
    _velocity :: Var Velocity,
    _acceleration :: Var Acceleration,
    _size :: Var Vector
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ do
        dt <- deltaTime
        a <- get acceleration self
        update velocity (.+. a .* dt) self
        entities <- allEntities
        boxes <- forM entities $ \entity -> do
            let physics = getFeature entity
            case physics of
                Just physics -> liftM Just (getBoundingBox physics)
                Nothing -> return Nothing
        s <- get size self
        v <- get velocity self
        update position (move (catMaybes boxes) s (v .* dt)) self

new :: Position -> Velocity -> Acceleration -> Vector -> Game Type
new position velocity acceleration size = 
    return Type .$. position .$. velocity .$. acceleration .$. size

modifyAcceleration :: (Acceleration -> Acceleration) -> Type -> Game ()
modifyAcceleration f self = update acceleration f self 

modifyVelocity :: (Velocity -> Velocity) -> Type -> Game ()
modifyVelocity f self = update velocity f self 

getAcceleration :: Type -> Game Acceleration
getAcceleration self = get acceleration self

getVelocity :: Type -> Game Velocity
getVelocity self = get velocity self

getPosition :: Type -> Game Position
getPosition self = get position self

getBoundingBox :: Type -> Game Box
getBoundingBox self = do
    p <- get position self
    s <- get size self
    return (boxAround p s)


move :: [Box] -> Vector -> Vector -> Position -> Position
move boxes size movement (x, y) =

    -- Move horizontally as far as possible (at most by vectorX movement)
    let startBox@((x1, y1), (x2, y2)) = boxAround (x, y) size in
    let boxes' = filter (not . overlap startBox) boxes in
    let x' = if vectorX movement <= 0 
        then
            let box = ((x1 + vectorX movement, y1), (x2, y2)) in
            let boxes'' = filter (overlap box) boxes' in
            let x1' = maximum (x1 + vectorX movement : map (vectorX . snd) boxes'') in
            min x1 (x1' + epsilon) + vectorX size / 2
        else
            let box = ((x1, y1), (x2 + vectorX movement, y2)) in
            let boxes'' = filter (overlap box) boxes' in
            let x2' = minimum (x2 + vectorX movement : map (vectorX . fst) boxes'') in
            max x2 (x2' - epsilon) - vectorX size / 2 in
            
    -- Then move vertically as far as possible (at most by vectorY movement)
    let startBox@((x1, y1), (x2, y2)) = boxAround (x', y) size in
    let boxes' = filter (not . overlap startBox) boxes in
    let y' = if vectorY movement <= 0 
        then
            let box = ((x1, y1 + vectorY movement), (x2, y2)) in
            let boxes'' = filter (overlap box) boxes' in
            let y1' = maximum (y1 + vectorY movement : map (vectorY . snd) boxes'') in
            min y1 (y1' + epsilon) + vectorY size / 2
        else
            let box = ((x1, y1), (x2, y2 + vectorY movement)) in
            let boxes'' = filter (overlap box) boxes' in
            let y2' = minimum (y2 + vectorY movement : map (vectorY . fst) boxes'') in
            max y2 (y2' - epsilon) - vectorY size / 2 in

    (x', y')
            

boxAround :: Position -> Vector -> Box
boxAround position size = ((position .-. size ./ 2), (position .+. size ./ 2))

overlap :: Box -> Box -> Bool
overlap (v1, v2) (u1, u2) = 
    overlapAxis (vectorX v1, vectorX v2) (vectorX u1, vectorX u2) &&
    overlapAxis (vectorY v1, vectorY v2) (vectorY u1, vectorY u2)
    where
        overlapAxis (start1, end1) (start2, end2) = start1 <= end2 && start2 <= end1

