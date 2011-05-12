import Feature
import qualified AsciiShooter.Entity.Player as Player
import qualified AsciiShooter.Feature.Position as Position
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
    runGame state (liftM spawn Player.new)
    newTime <- getCurrentTime
    lastTime <- newIORef newTime
    withWorld $ \world -> forever $ do
        input' <- input world
        oldTime <- readIORef lastTime
        newTime <- getCurrentTime
        writeIORef lastTime newTime
        atomically $ writeTVar deltaTimeVar (diffTime newTime oldTime)
        updateGameState state
        entities <- atomically $ readTVar entitiesVar
        output world WorldOutput { outputEntities = entities }

spawn :: Entity () -> Game (Entity ())
spawn entity = do
    state <- ask
    let entitiesVar = gameEntities state
    entities <- lift (readTVar entitiesVar)
    lift $ writeTVar entitiesVar (entity : entities)
    return entity

diffTime :: UTCTime -> UTCTime -> Double
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)

