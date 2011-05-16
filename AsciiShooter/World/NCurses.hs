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
import Data.Array.ST
import Data.Array.Unboxed
import Data.Word
import Data.Char (toLower)
import Data.Foldable (mapM_, forM_)
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.Trans
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
                keys <- untilM (isEmptyTChan channel) (readTChan channel)
                return (WorldInput { inputKeys = keys }),
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

untilM :: (Monad m) => (m Bool) -> m a -> m [a]
untilM predicate monad = do
    b <- predicate
    if not b 
        then do
            a <- monad
            as <- untilM predicate monad
            return (a : as)
        else
            return []

initializeColors :: Curses (Color -> ColorID)
initializeColors = do
    red <- newColorID ColorRed ColorBlack 1
    green <- newColorID ColorGreen ColorBlack 2
    blue <- newColorID ColorBlue ColorBlack 3
    yellow <- newColorID ColorYellow ColorBlack 4
    magenta <- newColorID ColorMagenta ColorBlack 5
    cyan <- newColorID ColorCyan ColorBlack 6
    black <- newColorID ColorBlack ColorBlack 7
    white <- newColorID ColorWhite ColorBlack 8
    return $ \color -> case color of 
        Red -> red
        Green -> green
        Blue -> blue
        Yellow -> yellow
        Magenta -> magenta
        Cyan -> cyan
        Black -> black
        White -> white
        Transparent -> black

draw :: Window -> (Color -> ColorID) -> WorldOutput -> Curses ()
draw window colors WorldOutput { outputSprites = sprites } = do
    (rows, columns) <- screenSize
    -- Workaround (rows - 1) because drawing on the bottom edge breaks NCurses
    let picture' = runSTUArray $ do
            picture <- background columns (rows - 1)
            mapM_ (drawEntity picture) (Map.elems sprites)
            return picture
    drawPicture window colors picture'

drawPicture :: Window -> (Color -> ColorID) -> UArray (Int, Int) Word32 -> Curses ()
drawPicture window colors picture = do
    updateWindow window $ do
        forM_ (assocs picture) $ \((x, y), c) -> do
            let (character, color) = toPixel c
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

