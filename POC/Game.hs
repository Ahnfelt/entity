module Game (
    GameState, 
    Game, 
    Entity,
    Var, variable, get, set, modify,
    module Entity
    ) where

import Entity

import Control.Concurrent.STM
import Control.Monad.Trans

data GameState = GameState {
    -- Your game state
    }

type Game a = GameMonad GameState a

type Entity a = EntityState GameState a


type Var a = TVar a

variable a = lift (newTVar a)

get a = lift (readTVar a)

set a b = lift (writeTVar a b)

modify f l = do
    v <- get l
    set l (f v)

