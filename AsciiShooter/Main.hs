import Feature
import qualified AsciiShooter.Entity.Tank as Tank
import AsciiShooter.World
import AsciiShooter.World.NCurses

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Control.Monad.Reader
import Data.Time.Clock
import Data.IORef

main = do
    entitiesVar <- newTVarIO []
    deltaTimeVar <- newTVarIO 0
    let state = GameState { gameEntities = entitiesVar, gameDeltaTime = deltaTimeVar }

    p <- runGame state (Tank.new (10, 40) (5, 0))
    runGame state (spawn p)

    p <- runGame state (Tank.new (10, 10) (5, 5))
    runGame state (spawn p)

    p <- runGame state (Tank.new (100, 50) (-10, -1.72))
    runGame state (spawn p)

    newTime <- getCurrentTime
    lastTime <- newIORef newTime

    drawTime <- newIORef 0
    updateTime <- newIORef 0

    withWorld $ \world -> forever $ do
        input' <- input world
        oldTime <- readIORef lastTime
        newTime <- getCurrentTime
        writeIORef lastTime newTime
        atomically $ writeTVar deltaTimeVar (diffTime newTime oldTime)
        updateGameState state
        entities <- atomically $ readTVar entitiesVar
        output world state

spawn :: Entity () -> Game (Entity ())
spawn entity = do
    state <- ask
    let entitiesVar = gameEntities state
    entities <- lift (readTVar entitiesVar)
    lift $ writeTVar entitiesVar (entity : entities)
    return entity

diffTime :: UTCTime -> UTCTime -> Double
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)

