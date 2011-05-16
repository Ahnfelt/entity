import Feature
import qualified AsciiShooter.Entity.Tank as Tank
import qualified AsciiShooter.Entity.Wall as Wall
import qualified AsciiShooter.Feature.Listener as Listener
import qualified AsciiShooter.Feature.Animation as Animation
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Sprite
import AsciiShooter.World
import AsciiShooter.World.NCurses

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import System.Random
import Data.Maybe
import qualified Data.Map as Map
import Data.Time.Clock
import Data.IORef

main = do
    nextKeyVar <- newTVarIO 0
    entitiesVar <- newTVarIO Map.empty
    generator <- newStdGen
    generatorVar <- newTVarIO generator
    currentTimeVar <- newTVarIO 0
    deltaTimeVar <- newTVarIO 0
    let state = GameState { 
        gameNextKey = nextKeyVar,
        gameEntities = entitiesVar, 
        gameGenerator = generatorVar,
        gameCurrentTime = currentTimeVar,
        gameDeltaTime = deltaTimeVar 
        }

    p <- runGame state (Tank.new 3 (10, 40) (5, 0))
    runGame state (spawn p)

    p <- runGame state (Tank.new 4 (10, 10) (5, 5))
    runGame state (spawn p)

    p <- runGame state (Tank.new 3 (100, 50) (-10, -1.72))
    runGame state (spawn p)

    p <- runGame state (Tank.new 1 (90, 20) (0, 0))
    runGame state (spawn p)

    p <- runGame state (Tank.new 2 (100, 30) (0, 0))
    runGame state (spawn p)

    p <- runGame state (Wall.new ((0, 0), (99.5, 4.5)))
    runGame state (spawn p)

    p <- runGame state (Wall.new ((120.5, 10.5), (130.5, 50.5)))
    runGame state (spawn p)

    newTime <- getCurrentTime
    lastTime <- newIORef newTime

    profileDraw <- profiler "draw"
    profileUpdate <- profiler "update"

    withWorld $ \world -> forever $ do

        input' <- input world

        oldTime <- readIORef lastTime
        newTime <- getCurrentTime
        writeIORef lastTime newTime
        atomically $ do
            let dt = diffTime newTime oldTime
            writeTVar deltaTimeVar dt
            t <- readTVar (gameCurrentTime state)
            writeTVar (gameCurrentTime state) (t + dt)

        entities <- atomically $ readTVar entitiesVar
        forM_ (inputKeys input') $ \key -> mapM_ (runGame state . fireInput key) (Map.elems entities)
        updateGameState state

        entities <- atomically $ readTVar entitiesVar
        sprites <- mapM (runGame state . toSprite) (Map.elems entities)

        output world WorldOutput { outputSprites = Map.fromList (catMaybes sprites) }


toSprite :: Entity () -> Game (Maybe (EntityKey, (Position, Sprite)))
toSprite entity = case getFeature entity of
    Just animation -> do
        pair <- Animation.getPositionedSprite animation
        return (Just (entityKey entity, pair))
    Nothing -> return Nothing

fireInput :: PlayerKey -> Entity () -> Game ()
fireInput playerKey entity = case getFeature entity of
    Just listener -> Listener.fireEvent playerKey listener
    Nothing -> return ()
    

diffTime :: UTCTime -> UTCTime -> Double
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)


profiler :: String -> IO (IO a -> IO a)
profiler name = do
    timeSpent <- newIORef 0
    return $ \monad -> do
        start <- getCurrentTime
        result <- monad
        stop <- getCurrentTime
        t <- readIORef timeSpent
        let t' = t + diffTime stop start
        writeIORef timeSpent t'
        putStrLn ("Time spent so far in " ++ name ++ ":" ++ show t')
        return result
        
