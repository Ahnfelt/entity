{-# LANGUAGE 
    MultiParamTypeClasses, 
    FlexibleContexts,
    FlexibleInstances, 
    OverlappingInstances,
    TypeSynonymInstances,
    TypeOperators,
    ScopedTypeVariables #-}

module Entity (
    EntityState,
    Eventually,
    GameMonad,
    object, method,
    Feature (..)
    ) where

import Control.Monad.Reader
import Control.Monad
import Control.Concurrent.STM
import Data.Dynamic
import Data.Maybe


{-|
    The type of all entities. The first argument is a heterogeneous list of 
    features, which allows us to maintain type safety.
-}
data EntityState s a = EntityState a [Dynamic] (GameMonad s ())


{-|
    Updates all the features of an entity.
-}
updateEntity (EntityState _ _ updater) = updater


{-|
    A wrapper that prevents the use of "this" until it has been fully constructed.
-}
newtype Eventually a = Eventually (TVar a)


{-|
    This type is the monad that updates take place in. Usually you'd make your
    own GameState data type and then alias:
        type Game a = GameMonad GameState a
-}
type GameMonad s a = ReaderT s STM a


{-|
    Corresponds to a constructor in an object oriented language.
    This is perhaps best illustrated with an example:
        new position = object $ \this -> do
            feature1 <- Feature1.new this position
            feature2 <- Feature2.new this (method myMethod this)
            return $ feature1 :*: feature2 :*: Nil
        
        myFeature this = do
            let feature1 = feature this
            ...
-}
object :: forall m l l' l'' s. 
    (MapApply ToDynamic l l', Homogeneous l' Dynamic,
    MapApply ToUpdater l l'', Homogeneous l'' (Maybe (GameMonad s ()))) =>  
    (Eventually (EntityState s l) -> GameMonad s l) -> GameMonad s (EntityState s l)
object function = do
    this <- lift $ newTVar (EntityState undefined undefined undefined)
    list <- function (Eventually this)
    let dynamics = homogeneous (mapApply ToDynamic list :: l')
    let updaters = catMaybes (homogeneous (mapApply ToUpdater list :: l'')) :: [GameMonad s ()]
    let entity = EntityState list dynamics (sequence_ updaters)
    lift $ writeTVar this entity
    return entity


{-|
    Wraps a function taking "this" so that it becomes a method.
    See object for an example.
-}
method :: (EntityState s a -> GameMonad s b) -> Eventually (EntityState s a) -> GameMonad s b
method function (Eventually entityVariable) = do
    entity <- lift $ readTVar entityVariable
    function entity


{-|
    You can get features directly when the type system can guarantee that
    the feature is actually present on the entity:
        let myFeature = feature myEntity
    
    When the type system can give no such guarantee, you can always get a
    Maybe MyFeature back that will be Just myFeature if it's present:
        case feature myEntity of
            Just myFeature -> ...
            Nothing -> ...
-}
class Feature a e where
    feature :: e -> a

instance (Typeable a) => Feature (Maybe a) (EntityState s l) where
    feature (EntityState _ [] _) = Nothing
    feature (EntityState _ (d:ds) _) = fromMaybe (feature (EntityState undefined ds undefined)) (fromDynamic d)

instance (Element a l, Typeable a) => Feature a (EntityState s l) where
    feature (EntityState l _ _) = element l


---------- Functions for heterogeneous list ----------


data ToDynamic = ToDynamic

instance Typeable a => Apply ToDynamic (u, a) Dynamic where
    apply ToDynamic = toDyn . snd


data ToUpdater = ToUpdater

instance Apply ToUpdater (Maybe (GameMonad s ()), a) (Maybe (GameMonad s ())) where
    apply ToUpdater = fst


---------- Heterogeneous lists ----------


infixr 0 :*:

{-| Cons and Nil for heterogeneous lists. -}
data a :*: b = a :*: b
data Nil = Nil


class Element a l where
    {-| Takes the first element with the expected type out of the list -}
    element :: l -> a

instance Element a (a :*: l) where
    element (a :*: _) = a

instance Element a l => Element a (b :*: l) where
    element (_ :*: l) = element l


class Homogeneous l a where
    {-| Converts to an ordinary list, if the heterogeneous list is also homogeneous. -}
    homogeneous :: l -> [a]

instance Homogeneous l a => Homogeneous (a :*: l) a where
    homogeneous (a :*: l) = a : homogeneous l

instance Homogeneous Nil a where
    homogeneous Nil = []


class Apply f a b where
    {-| A type class for functions that can be mapped over heterogeneous lists. -}
    apply :: f -> a -> b


class MapApply f l l' where
    {-| Map for heterogeneous lists. -}
    mapApply :: f -> l -> l'

instance (Apply f a b, MapApply f l l') => MapApply f (a :*: l) (b :*: l') where
    mapApply f (a :*: l) = apply f a :*: mapApply f l

instance MapApply f Nil Nil where
    mapApply _ Nil = Nil


