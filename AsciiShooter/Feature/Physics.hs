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
    _solid :: Bool,
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
                return (Just (getL solid physics, (box, [entity])))
            Nothing -> return Nothing
        let boxes' = catMaybes boxes
        let solidBoxes = map snd (filter fst boxes')
        let allBoxes = map snd boxes'

        s <- get size self
        v <- get velocity self
        p <- get position self

        let (p', hits) = move solidBoxes s (v .* dt) p
        let hits' = touches allBoxes s p p'
        let hits'' = 
                nubBy (\a b -> entityKey a == entityKey b) $
                filter (\e -> entityKey e /= getL key self) $
                hits ++ hits'

        set position p' self

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
    this feature automatically applies acceleration and velocity to move as far as it
    is supposed to, but without crossing other solid entities. The size is the size 
    of the bounding box around the object, centered at the position. The key is the
    key of the entity that uses this instance of the feature.
    
    Only solid objects can stand in the way of other (solid or non-solid) objects. 
    However, hit events are sent out for all objects that are hit or overlap the path,
    regardless of solidity of either object.
-}
new :: Position -> Velocity -> Acceleration -> Vector -> Bool -> EntityKey -> Game Type
new position velocity acceleration size solid key = 
    return Type .$. position .$. velocity .$. acceleration .$. size .$. solid .$. key

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
            

touches :: [(Box, [a])] -> Vector -> Position -> Position -> [a]
touches boxes (x, y) (x1, y1) (x2, y2) =

    let horizontalBox = boxAround ((x1 + x2) / 2, y1) (x + abs (x2 - x1), y) in
    let horizontalBoxes = filter (overlap horizontalBox . fst) boxes in
    let horizontalHits = map snd horizontalBoxes in
    
    let verticalBox = boxAround (x2, (y1 + y2) / 2) (x, y + abs (y2 - y1)) in
    let verticalBoxes = filter (overlap verticalBox . fst) boxes in
    let verticalHits = map snd verticalBoxes in

    concat (horizontalHits ++ verticalHits)

