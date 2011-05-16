{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Physics (
    Type, new, 
    modifyAcceleration, modifyVelocity, 
    getDistanceTraveled,
    getAcceleration, getVelocity, getPosition,
    getBoundingBox, getCanBlock, getCanBeBlocked,
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
    _distanceTraveled :: Var Double,
    _size :: Var Vector,
    _canBlock :: Bool,
    _canBeBlocked :: Bool,
    _key :: EntityKey
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type where
    updater self = Just $ do
        dt <- deltaTime
        a <- get acceleration self
        update velocity (.+. a .* dt) self

        entities <- allEntities
        boxes <- forM entities $ \entity -> case getFeature entity of
            Just physics -> do
                box <- getBoundingBox physics
                if (entityKey entity /= getL key self)
                    then return (Just (getL canBlock physics, (box, [entity])))
                    else return Nothing
            Nothing -> return Nothing
        let boxes' = catMaybes boxes
        let blockingBoxes = map snd (filter fst boxes')
        let allBoxes = map snd boxes'

        s <- get size self
        v <- get velocity self
        p <- get position self

        let instant = not (getL canBlock self)
        let (p', hits) = if getL canBeBlocked self
                then move blockingBoxes instant s (v .* dt) p
                else (p .+. v .* dt, [])
        let hits' = touches allBoxes s p p'
        let hits'' = 
                nubBy (\a b -> entityKey a == entityKey b) $
                hits ++ hits'

        set position p' self
        
        let d = vectorLength (p .-. p') 
        update distanceTraveled (d +) self

        entity <- entityByKey (getL key self)
        case entity of
            Just entity -> do
                forM_ hits'' $ \entity' -> do
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


{-|
    The position, velocity and acceleration should be self-explanatory, but note that
    this feature automatically applies acceleration and velocity to move the object. 
    The size is the size of the bounding box around the object, centered at the 
    position. The key is the key of the entity that uses this instance of the feature.

    Movement is automatically confined so that a canBeBlocked object is never moved
    into a canBlock object. However, hit events are sent out for all objects that are 
    hit or overlap the path, regardless of the blocking properties of either object.
-}
new :: Position -> Velocity -> Acceleration -> Vector -> Bool -> Bool -> EntityKey -> Game Type
new position velocity acceleration size canBlock canBeBlocked key = 
    return Type .$. position .$. velocity .$. acceleration .$. (0 :: Double) .$. size .$. canBlock .$. canBeBlocked .$. key

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

getDistanceTraveled :: Type -> Game Double
getDistanceTraveled self = get distanceTraveled self

getBoundingBox :: Type -> Game Box
getBoundingBox self = do
    p <- get position self
    s <- get size self
    return (boxAround p s)

getCanBlock :: Type -> Bool
getCanBlock self = getL canBlock self

getCanBeBlocked :: Type -> Bool
getCanBeBlocked self = getL canBeBlocked self


move :: [(Box, [a])] -> Bool -> Vector -> Vector -> Position -> (Position, [a])
move boxes instant size movement (x, y) =

    -- Move horizontally as far as possible (at most by vectorX movement)
    let startBox@((x1, y1), (x2, y2)) = boxAround (x, y) size in
    let boxes' = exclude startBox boxes in
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
    let boxes' = exclude startBox boxes in
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
    
    where
        exclude startBox boxes 
            | instant = boxes
            | otherwise = filter (not . overlap startBox . fst) boxes

touches :: [(Box, [a])] -> Vector -> Position -> Position -> [a]
touches boxes (x, y) (x1, y1) (x2, y2) =

    let horizontalBox = boxAround ((x1 + x2) / 2, y1) (x + abs (x2 - x1), y) in
    let horizontalBoxes = filter (overlap horizontalBox . fst) boxes in
    let horizontalHits = map snd horizontalBoxes in
    
    let verticalBox = boxAround (x2, (y1 + y2) / 2) (x, y + abs (y2 - y1)) in
    let verticalBoxes = filter (overlap verticalBox . fst) boxes in
    let verticalHits = map snd verticalBoxes in

    concat (horizontalHits ++ verticalHits)

