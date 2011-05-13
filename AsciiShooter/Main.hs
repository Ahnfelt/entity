import Feature
import qualified AsciiShooter.Entity.Tank as Tank
import qualified AsciiShooter.Feature.Listener as Listener
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

    p <- runGame state (Tank.new 1 (10, 40) (5, 0))
    runGame state (spawn p)

    p <- runGame state (Tank.new 2 (10, 10) (5, 5))
    runGame state (spawn p)

    p <- runGame state (Tank.new 3 (100, 50) (-10, -1.72))
    runGame state (spawn p)

    p <- runGame state (Tank.new 3 (90, 20) (0, 0))
    runGame state (spawn p)

    p <- runGame state (Tank.new 3 (90, 30) (0, 0))
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

        entities <- atomically $ readTVar entitiesVar
        forM_ (inputKeys input') $ \key -> mapM_ (runGame state . fireInput key) entities
        updateGameState state

        output world state


fireInput :: PlayerKey -> Entity () -> Game ()
fireInput playerKey entity = case getFeature entity of
    Just listener -> Listener.fireEvent playerKey listener
    Nothing -> return ()
    

diffTime :: UTCTime -> UTCTime -> Double
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)

