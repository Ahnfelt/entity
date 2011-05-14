{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- NOTE: UndecidableInstances is required by the Combine type class,
--       which is a restricted copy of HAppend in the HList module.
--       It can be removed yielding a slightly less precise system.
--       It is also used for the Has type class synonym.

module Feature (
    GameState (..), Game, runGame, Var, 
    updateGameState, 
    deltaTime, allEntities, spawn, unspawn, entityByKey,
    (.:.), Combine ((+++)), 
    EntityKey, entityKey,
    Entity, toEntity, updateEntity, 
    requireFeature, RequireFeatures (..), 
    getFeature, GetFeatures (..), 
    Updateable (..),
    Convert (..), (.$.), 
    object, method, 
    get, set, update,
    Has, has, nil) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad
import Data.HList
import Data.Dynamic
import Data.Maybe
import Data.Record.Label
import Data.Map (Map)
import qualified Data.Map as Map

-- Game specific stuff (well, not *that* specific)

data GameState = GameState {
    gameNextKey :: TVar EntityKey,
    gameEntities :: TVar (Map EntityKey (Entity ())),
    gameDeltaTime :: TVar Double
    }

updateGameState :: GameState -> IO ()
updateGameState state = do
    entities <- atomically $ readTVar (gameEntities state)
    mapM_ (runGame state . updateEntity) (Map.elems entities)

deltaTime :: Game Double
deltaTime = do
    state <- ask
    lift $ readTVar (gameDeltaTime state)

allEntities :: Game [Entity ()]
allEntities = do
    state <- ask
    entities <- lift $ readTVar (gameEntities state)
    return (Map.elems entities)

entityByKey :: EntityKey -> Game (Maybe (Entity ()))
entityByKey key = do
    state <- ask
    entities <- lift $ readTVar (gameEntities state)
    return (Map.lookup key entities)

spawn :: Entity () -> Game (Entity ())
spawn entity@(Entity key _ _) = do
    state <- ask
    entities <- lift (readTVar (gameEntities state))
    lift $ writeTVar (gameEntities state) (Map.insert key entity entities)
    return entity

unspawn :: Entity a -> Game ()
unspawn (Entity key _ _) = do
    state <- ask
    entities <- lift (readTVar (gameEntities state))
    lift $ writeTVar (gameEntities state) (Map.delete key entities)


-- Reusable stuff

type EntityKey = Int

type Game a = ReaderT GameState STM a

type Var a = TVar a

runGame :: GameState -> Game a -> IO a
runGame s m = atomically $ runReaderT m s


class Convert a b where
    convert :: a -> Game b

instance Convert a a where
    convert = return

instance Convert a (Var a) where
    convert a = lift $ newTVar a

(.$.) :: (Convert a a') => Game (a' -> b) -> a -> Game b
f .$. v = do 
    f' <- f
    v' <- convert v
    return $ f' v'


object :: (Var (Entity p) -> EntityKey -> Game (Entity p)) -> Game (Entity ())
object constructor = do
    key <- do
        state <- ask
        key <- lift $ readTVar (gameNextKey state)
        lift $ writeTVar (gameNextKey state) (key + 1)
        return key
    this <- lift $ newTVar (Entity key undefined undefined)
    result <- constructor this key
    let Entity _ m ds = result
    lift $ writeTVar this (Entity key m ds)
    return (Entity key m ds)


class Method f where
    method :: (Entity p -> f) -> Var (Entity p) -> f

instance Method (Game a) where
    method function this = do
        this' <- lift $ readTVar this
        function this'

instance Method (b -> Game a) where
    method function this = \b -> do
        this' <- lift $ readTVar this
        function this' b 

instance Method (c -> b -> Game a) where
    method function this = \c b -> do
        this' <- lift $ readTVar this
        function this' c b

instance Method (d -> c -> b -> Game a) where
    method function this = \d c b -> do
        this' <- lift $ readTVar this
        function this' d c b 


get :: (record :-> Var value) -> record -> Game value
get label record = lift $ readTVar (getL label record)

set :: (record :-> Var value) -> value -> record -> Game ()
set label value record = lift $ writeTVar (getL label record) value

update :: (record :-> Var value) -> (value -> value) -> record -> Game ()
update label f record = do
    let tvar = getL label record
    value <- lift $ readTVar tvar
    lift $ writeTVar tvar (f value)


updateEntity :: Entity p -> Game ()
updateEntity (Entity _ u _) = u


data Entity p = Entity EntityKey (Game ()) [Dynamic]

entityKey :: Entity a -> EntityKey
entityKey (Entity key _ _) = key


class Updateable e where
    updater :: e -> Maybe (Game ())
    updater _ = Nothing
    

infixr 6 .:.
(.:.) :: (HOccursNot e l, HExtend e l l') => e -> l -> l'
a .:. b = a .*. b


class Combine l l' l'' | l l' -> l'' where
    (+++) :: l -> l' -> l''

instance HList l => Combine HNil l l where
    HNil +++ l = l

instance (HList l, Combine l l' l'', HOccursNot x l') => Combine (HCons x l) l' (HCons x l'') where
    HCons x l +++ l' = HCons x (l +++ l')


class HOccurs e l => Has e l
instance HOccurs e l => Has e l
has l = hOccurs l
nil = HNil

data ToDynamic = ToDynamic
data ToUpdater = ToUpdater

instance Typeable a => Apply ToDynamic a Dynamic where 
    apply ToDynamic a = toDyn a

instance Updateable a => Apply ToUpdater a (Maybe (Game ())) where 
    apply ToUpdater a = updater a

toEntity :: (HMapOut ToDynamic r Dynamic, HMapOut ToUpdater r (Maybe (Game ()))) => r -> Entity r
toEntity l = Entity undefined (sequence_ $ catMaybes us) ds
    where
        ds = hMapOut ToDynamic l :: [Dynamic]
        us = hMapOut ToUpdater l :: [Maybe (Game ())]

requireFeature :: (Typeable e, Updateable e, Has e p) => Entity p -> e
requireFeature e = let Just e' = getFeature e in e'

getFeature :: (Typeable e, Updateable e) => Entity p -> Maybe e
getFeature (Entity _ _ ds) = case catMaybes $ map fromDynamic ds of
    e:_ -> Just e
    [] -> Nothing

class GetFeatures l => RequireFeatures p l where
    requireFeatures :: Entity p -> l

instance RequireFeatures p HNil where
    requireFeatures _ = HNil

instance (RequireFeatures p l, Typeable e, Has p e) => RequireFeatures p (HCons e l) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, 
    Has p e1, Has p e2) => RequireFeatures p (e1, e2) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, Typeable e3, 
    Has p e1, Has p e2, Has p e3) => RequireFeatures p (e1, e2, e3) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, 
    Has p e1, Has p e2, Has p e3, Has p e4) => RequireFeatures p (e1, e2, e3, e4) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, 
    Has p e1, Has p e2, Has p e3, Has p e4, Has p e5) => RequireFeatures p (e1, e2, e3, e4, e5) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6, 
    Has p e1, Has p e2, Has p e3, Has p e4, Has p e5, Has p e6) => RequireFeatures p (e1, e2, e3, e4, e5, e6) where
    requireFeatures e = let Just e' = getFeatures e in e'

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6, Typeable e7, 
    Has p e1, Has p e2, Has p e3, Has p e4, Has p e5, Has p e6, Has p e7) => RequireFeatures p (e1, e2, e3, e4, e5, e6, e7) where
    requireFeatures e = let Just e' = getFeatures e in e'

class GetFeatures l where
    getFeatures :: Entity p -> Maybe l

instance GetFeatures HNil where
    getFeatures _ = Just HNil

instance (GetFeatures l, Typeable e) => GetFeatures (HCons e l) where
    getFeatures e@(Entity _ _ ds) = case catMaybes $ map fromDynamic ds of
        e1:_ -> case getFeatures e of
            Just l -> Just (HCons e1 l)
            Nothing -> Nothing
        [] -> Nothing

instance (Typeable e1, Typeable e2) => GetFeatures (e1, e2) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 HNil)) -> Just (e1, e2)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3) => GetFeatures (e1, e2, e3) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 (HCons e3 HNil))) -> Just (e1, e2, e3)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4) => GetFeatures (e1, e2, e3, e4) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 HNil)))) -> Just (e1, e2, e3, e4)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5) => GetFeatures (e1, e2, e3, e4, e5) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 HNil))))) -> Just (e1, e2, e3, e4, e5)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6) => GetFeatures (e1, e2, e3, e4, e5, e6) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 HNil)))))) -> Just (e1, e2, e3, e4, e5, e6)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6, Typeable e7) => GetFeatures (e1, e2, e3, e4, e5, e6, e7) where
    getFeatures e@(Entity _ _ ds) = case getFeatures e of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 (HCons e7 HNil))))))) -> Just (e1, e2, e3, e4, e5, e6, e7)
        Nothing -> Nothing

