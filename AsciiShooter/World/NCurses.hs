module AsciiShooter.World.NCurses (withWorld) where

import Feature
import AsciiShooter.World
import AsciiShooter.World.Ascii
import AsciiShooter.Key

import UI.NCurses hiding (Key, Color, Event)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad hiding (mapM_, forM_)
import Data.Array.Diff
import Data.Char (toLower)
import Data.Foldable 
import Control.Monad.Trans
import qualified Data.Text as T
import Prelude hiding (mapM_)
import Debug.Trace

withWorld gameFunction = do
        channel <- newTChanIO
        stateVariable <- newEmptyMVar
        doneVariable <- newEmptyMVar
        stopVariable <- newEmptyMVar
        forkIO $ runCurses $ do 
            window <- defaultWindow
            setEcho False
            colors <- initializeColors
            loop window colors channel stateVariable doneVariable stopVariable
        result <- gameFunction $ World {
            input = atomically $ do
                empty <- isEmptyTChan channel
                if empty 
                    then return (WorldInput { inputKeys = [] })
                    else do
                        state <- readTChan channel
                        return (WorldInput { inputKeys = [state] }),
            output = \state -> do
                putMVar stateVariable state
                takeMVar doneVariable
            }
        putMVar stopVariable ()
        return result
        where
            loop :: Window -> (Color -> ColorID) -> TChan PlayerKey -> MVar WorldOutput -> MVar () -> MVar () -> Curses ()
            loop window colors channel stateVariable doneVariable stopVariable = do
                continue <- liftIO $ isEmptyMVar stopVariable
                when continue $ do
                    key <- getKey window
                    liftIO $ mapM_ (atomically . writeTChan channel) key
                    state <- liftIO $ tryTakeMVar stateVariable
                    case state of
                        Just state -> do 
                            draw window colors state
                            liftIO $ putMVar doneVariable ()
                        Nothing -> return ()
                    loop window colors channel stateVariable doneVariable stopVariable

initializeColors :: Curses (Color -> ColorID)
initializeColors = do
    red <- newColorID ColorRed ColorBlack 1
    green <- newColorID ColorGreen ColorBlack 2
    black <- newColorID ColorBlack ColorBlack 3
    blue <- newColorID ColorBlue ColorBlack 4
    return $ \color -> case color of 
        Transparent -> black
        Green -> green
        Blue -> blue
        _ -> red

draw :: Window -> (Color -> ColorID) -> WorldOutput -> Curses ()
draw window colors worldOutput = do
    (rows, columns) <- screenSize
    -- Workaround (rows - 1) because drawing on the bottom edge breaks NCurses
    picture' <- liftIO $ runGame worldOutput $ do
        let picture = background columns (rows - 1)   
        entities <- lift $ readTVar (gameEntities worldOutput)
        foldM drawEntity picture entities
    drawPicture window colors picture'

drawPicture :: Window -> (Color -> ColorID) -> Picture -> Curses ()
drawPicture window colors picture = do
    updateWindow window $ do
        forM_ (assocs picture) $ \((x, y), (character, color)) -> do
            moveCursor (fromIntegral y) (fromIntegral x)
            setColor (colors color)
            drawText (T.pack ([character]))
    render


getKey :: Window -> Curses (Maybe PlayerKey)
getKey window = do
    event <- getEvent window (Just 0)
    case event of
        Just (EventSpecialKey key) -> 
            case key of
                KeyUpArrow -> return (Just (1, KeyNorth))
                KeyDownArrow -> return (Just (1, KeySouth))
                KeyLeftArrow -> return (Just (1, KeyWest))
                KeyRightArrow -> return (Just (1, KeyEast))
                KeyHome -> return (Just (1, KeyBreak))
                KeyEnd -> return (Just (1, KeyFire))
                _ -> return Nothing
        Just (EventCharacter character) -> 
            case toLower character of
                'r' -> return (Just (2, KeyNorth))
                'f' -> return (Just (2, KeySouth))
                'd' -> return (Just (2, KeyWest))
                'g' -> return (Just (2, KeyEast))
                'q' -> return (Just (2, KeyBreak))
                'a' -> return (Just (2, KeyFire))
                _ -> return Nothing
        _ -> return Nothing

