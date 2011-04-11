--module AsciiShooter.Terminal where

import UI.NCurses hiding (Key, Color)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.Text hiding (map)
import Data.Array.Diff
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data Color = Red

data Key = KeyUp | KeyDown | KeyLeft | KeyRight | KeyFire deriving (Eq, Ord, Show)

type Picture = DiffArray (Int, Int) (Char, Color)

main = runCurses $ do
    window <- defaultWindow
    (rows, columns) <- screenSize
    let picture = listArray ((0, 0), (fromIntegral columns - 1, fromIntegral rows - 1)) (repeat (' ', Red))
    pictureVar <- liftIO (newTVarIO picture)
    keyChan <- liftIO newTChanIO
    liftIO $ forkIO (controller pictureVar keyChan)
    drawListenLoop window pictureVar keyChan

draw :: Window -> (Color -> ColorID) -> Picture -> Curses ()
draw window colors picture = do
    updateWindow window $ do
        forM_ (assocs picture) $ \((x, y), (character, color)) -> do
            moveCursor (fromIntegral y) (fromIntegral x)
            --trace (show (x, y)) $ do
            --setColor (colors color)
            drawText (pack [character])
    render


listener :: Window -> TChan Key -> Curses ()
listener window keyChan = do
    event <- getEvent window (Just 0)
    liftIO $ atomically $ do
        case event of
            Just (EventSpecialKey key) -> 
                case key of
                    KeyUpArrow -> writeTChan keyChan KeyUp
                    KeyDownArrow -> writeTChan keyChan KeyDown
                    KeyLeftArrow -> writeTChan keyChan KeyLeft
                    KeyRightArrow -> writeTChan keyChan KeyRight
                    _ -> return ()
            Just (EventCharacter character) -> 
                case character of
                    ' ' -> writeTChan keyChan KeyFire
                    _ -> return ()
            _ -> return ()


drawListenLoop :: Window -> TVar Picture -> TChan Key -> Curses ()
drawListenLoop window pictureVar keyChan = do 
    red <- newColorID ColorRed ColorBlack 1
    loop window (const red)
    where
        loop :: Window -> (Color -> ColorID) -> Curses ()
        loop window colors = do
            picture <- liftIO (readTVarIO pictureVar)
            draw window colors picture
            listener window keyChan
            loop window colors


controller :: TVar Picture -> TChan Key -> IO ()
controller pictureVar keyChan = loop (10, 10)
    where
        loop (x, y) = do
            tankLocation <- atomically $ do
                key <- readTChan keyChan
                let (sprite, tankLocation) = case key of
                        KeyUp -> (tankSpriteUp, (x, y + 1))
                let sprite' = translateSprite tankLocation sprite
                picture <- readTVar pictureVar
                writeTVar pictureVar (picture // sprite')
                return tankLocation
            loop tankLocation

translateSprite (x, y) sprite = 
    map (first (\(x', y') -> (x + x', y + y'))) sprite
        
tankSpriteUp = 
    let tank = [
            ((0,0),'¤'), ((1,0),'|'), ((2,0),'|'), ((3,0),'¤'), 
            ((0,1),'¤'), ((1,1),'('), ((2,1),')'), ((3,1),'¤'), 
            ((0,2),'¤'), ((1,2),'-'), ((2,2),'-'), ((3,2),'¤')] in
    map (second (\c -> (c, Red))) tank

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

