{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Physics (
    Type, new, 
    modifyAcceleration, modifyVelocity,
    getAcceleration, getVelocity, getPosition,
    getBoundingBox,
    Hit (..)
    ) where

import Feature
import AsciiShooter.Utilities.Mechanics
import qualified AsciiShooter.Feature.Listener as Listener

import Data.Typeable
import Data.Record.Label
import Data.Maybe
import Data.Ord
import Data.List
import Control.Monad

data Hit = Hit { 
    receiversFault :: Bool,
    hitEntity :: Entity ()
    } deriving (Typeable)

data Type = Type {
    _position :: Var Position,
    _velocity :: Var Velocity,
    _acceleration :: Var Acceleration,
    _size :: Var Vector,
    _key :: EntityKey
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
                Just physics -> do
                    box <- getBoundingBox physics
                    return (Just (box, [entity]))
                Nothing -> return Nothing
        s <- get size self
        v <- get velocity self
        p <- get position self
        let (p', hits) = move (catMaybes boxes) s (v .* dt) p
        set position p' self
        entity <- entityByKey (getL key self)
        case entity of
            Just entity -> do
                forM_ hits $ \entity' -> do
                    case getFeature entity of
                        Just listener -> do
                            let hit = Hit { receiversFault = True, hitEntity = entity' }
                            Listener.fireEvent hit listener
                        Nothing -> return ()
                    case getFeature entity' of
                        Just listener -> do
                            let hit = Hit { receiversFault = False, hitEntity = entity }
                            Listener.fireEvent hit listener
                        Nothing -> return ()
            Nothing -> return ()

new :: Position -> Velocity -> Acceleration -> Vector -> EntityKey -> Game Type
new position velocity acceleration size key = 
    return Type .$. position .$. velocity .$. acceleration .$. size .$. key

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


move :: [(Box, [a])] -> Vector -> Vector -> Position -> (Position, [a])
move boxes size movement (x, y) =

    -- Move horizontally as far as possible (at most by vectorX movement)
    let startBox@((x1, y1), (x2, y2)) = boxAround (x, y) size in
    let boxes' = filter (not . overlap startBox . fst) boxes in
    let (x', hitX) = if vectorX movement <= 0 
            then
                let box = ((x1 + vectorX movement, y1), (x2, y2)) in
                let boxes'' = filter (overlap box . fst) boxes' in
                let boxes''' = map (\(b, e) -> (vectorX (snd b), e)) boxes'' in
                let stop = (x1 + vectorX movement, []) in
                let (x1', hit) = maximumBy (comparing fst) (stop : boxes''') in
                (min x1 (x1' + epsilon) + vectorX size / 2, hit)
            else
                let box = ((x1, y1), (x2 + vectorX movement, y2)) in
                let boxes'' = filter (overlap box . fst) boxes' in
                let boxes''' = map (\(b, e) -> (vectorX (fst b), e)) boxes'' in
                let stop = (x2 + vectorX movement, []) in
                let (x2', hit) = minimumBy (comparing fst) (stop : boxes''') in
                (max x2 (x2' - epsilon) - vectorX size / 2, hit) in
            
    -- Then move vertically as far as possible (at most by vectorY movement)
    let startBox@((x1, y1), (x2, y2)) = boxAround (x', y) size in
    let boxes' = filter (not . overlap startBox . fst) boxes in
    let (y', hitY) = if vectorY movement <= 0 
            then
                let box = ((x1, y1 + vectorY movement), (x2, y2)) in
                let boxes'' = filter (overlap box . fst) boxes' in
                let boxes''' = map (\(b, e) -> (vectorY (snd b), e)) boxes'' in
                let stop = (y1 + vectorY movement, []) in
                let (y1', hit) = maximumBy (comparing fst) (stop : boxes''') in
                (min y1 (y1' + epsilon) + vectorY size / 2, hit)
            else
                let box = ((x1, y1), (x2, y2 + vectorY movement)) in
                let boxes'' = filter (overlap box . fst) boxes' in
                let boxes''' = map (\(b, e) -> (vectorY (fst b), e)) boxes'' in
                let stop = (y2 + vectorY movement, []) in
                let (y2', hit) = minimumBy (comparing fst) (stop : boxes''') in
                (max y2 (y2' - epsilon) - vectorY size / 2, hit) in

    ((x', y'), hitX ++ hitY)
            


