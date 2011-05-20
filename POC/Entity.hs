{-# LANGUAGE 
    MultiParamTypeClasses, 
    FlexibleContexts,
    FlexibleInstances, 
    OverlappingInstances,
    TypeSynonymInstances,
    TypeOperators #-}

module Entity (
    EntityState,
    Eventually,
    GameMonad,
    updateEntities,
    object, method,
    Feature (..),
    Has, (:*:), (.:.), Nil (..)
    ) where

import Control.Monad.Reader
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.QSemN
import Data.Dynamic
import Data.Maybe


{-|
    The type of all entities. The first argument is a heterogeneous list of 
    features, which allows us to maintain type safety.
-}
data EntityState s a = EntityState a [Dynamic] [GameMonad s ()]


{-|
    Given a state, updates (the features of) the entities in parallel. 
    Each individual feature update is atomic, in the STM sense.
    It returns when all the entities in the list have been updated.
-}
updateEntities :: s -> [EntityState s a] -> IO ()
updateEntities state entities = do
    counter <- newQSemN 0
    forM_ entities $ \entity -> do
        let (EntityState _ _ updaters) = entity
        forkIO $ do
            forM_ updaters $ \updater -> do
                atomically $ runReaderT updater state
            signalQSemN counter 1
    waitQSemN counter (length entities)


{-|
    A wrapper that discourages the use of "this" until it has been fully constructed.
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
object :: (MapDynamic l, MapUpdater l s, MapSecond l l') =>  
    (Eventually (EntityState s l') -> GameMonad s l) -> GameMonad s (EntityState s l')
object function = do
    this <- lift $ newTVar (EntityState undefined undefined undefined)
    list <- function (Eventually this)
    let dynamics = mapDynamic list
    let updaters = catMaybes (mapUpdater list)
    let list' = mapSecond list
    let entity = EntityState list' dynamics updaters
    lift $ writeTVar this entity
    return entity


{-|
    Wraps a function taking "this" so that it becomes a method.
    You mustn't call the function returned by method in a constructor (in a feature or entity).
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

instance Typeable a => Feature (Maybe a) (EntityState s l) where
    feature (EntityState _ [] _) = Nothing
    feature (EntityState _ (d:ds) _) = fromMaybe (feature (EntityState undefined ds undefined)) (fromDynamic d)

instance Has a l => Feature a (EntityState s l) where
    feature (EntityState l _ _) = element l


---------- Functions for heterogeneous list ----------


class MapSecond l l' where
    mapSecond :: l -> l'

instance MapSecond l l' => MapSecond ((a, b) :*: l) (b :*: l') where
    mapSecond ((_, b) :*: l) = b :*: mapSecond l

instance MapSecond Nil Nil where
    mapSecond Nil = Nil


class MapDynamic l where
    mapDynamic :: l -> [Dynamic]

instance (Typeable b, MapDynamic l) => MapDynamic ((a, b) :*: l) where
    mapDynamic ((_, b) :*: l) = toDyn b : mapDynamic l

instance MapDynamic Nil where
    mapDynamic Nil = []


class MapUpdater l s where
    mapUpdater :: l -> [Maybe (GameMonad s ())]

instance MapUpdater l s => MapUpdater ((Maybe (GameMonad s ()), b) :*: l) s where
    mapUpdater ((a, _) :*: l) = a : mapUpdater l

instance MapUpdater Nil s where
    mapUpdater Nil = []


---------- Heterogeneous lists ----------


infixr 0 :*:, .:.

{-| Cons and Nil for heterogeneous lists. -}
data a :*: l = a :*: l
data Nil = Nil


{-| A requirement that a heterogeneous list contains an element of type a. -}
class Has a l where
    {-| Returns the first element with the expected type from the list. -}
    element :: l -> a

instance Has a (a :*: l) where
    element (a :*: _) = a

instance Has a l => Has a (b :*: l) where
    element (_ :*: l) = element l


{-| A requirement that a heterogeneous list does not contain an element of type a. -}
class HasNot a l where
    {-| Prepends an element to a list not already containing an element of that type. -}
    (.:.) :: a -> l -> (a :*: l)

instance HasNot a () => HasNot a (a :*: l) where
    (.:.) = error "Can never happen since HasNot a () fails."

instance HasNot a l => HasNot a (a' :*: l) where
    (.:.) = (:*:)

instance HasNot a Nil where
    (.:.) = (:*:)

