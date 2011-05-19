module Game (
    GameState, 
    Game, 
    Entity, 
    module Entity
    ) where

import Entity

data GameState = GameState {
    -- Your game state
    }

type Game a = GameMonad GameState a

type Entity a = EntityState GameState a

