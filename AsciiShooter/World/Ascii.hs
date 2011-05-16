module AsciiShooter.World.Ascii (
    Color (..),
    background, drawEntity,
    toPixel
    ) where

import Feature
import qualified AsciiShooter.Feature.Physics as Physics
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.World
import qualified AsciiShooter.Sprite as Sprite

import qualified Data.Set as Set
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable 
import Data.Word
import Data.Bits
import Data.Set (Set)
import Data.List (transpose)
import Prelude hiding (maximum)

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Black | White | Transparent deriving (Eq, Ord, Show, Enum)
type Picture s = STUArray s (Int, Int) Word32
data Sprite = Sprite Int Int [((Int, Int), (Char, Color))]

writePixel :: Picture s -> (Int, Int) -> (Char, Color) -> ST s ()
writePixel array i p = writeArray array i (fromPixel p)

fromPixel :: (Char, Color) -> Word32
fromPixel (char, color) = (fromIntegral (fromEnum char `shiftL` 8) .|. fromIntegral (fromEnum color))

toPixel :: Word32 -> (Char, Color)
toPixel p = (toEnum (fromIntegral (p `shiftR` 8)), toEnum (fromIntegral (p .&. 255)))

playerColor 1 = Red
playerColor 2 = Green
playerColor 3 = Blue
playerColor 4 = Yellow

drawEntity :: Picture s -> (Position, Sprite.Sprite) -> ST s ()
drawEntity picture (position, sprite) = case sprite of
    Sprite.Tank direction state player -> drawTank picture (playerColor player) (position, direction) state
    Sprite.Projectile player -> drawProjectile picture (playerColor player) position
    Sprite.Wall size -> drawWall picture position size
    Sprite.Debree -> drawDebree picture position

background :: Integral a => a -> a -> ST s (Picture s)
background width height = do
    newArray ((0, 0), (fromIntegral width - 1, fromIntegral height - 1)) (fromPixel (' ', Transparent))
    

translatePoints :: (Int, Int) -> [((Int, Int), (Char, Color))] -> [((Int, Int), (Char, Color))]
translatePoints (x, y) sprite = 
    map ((\((x', y'), c) -> ((x + x', y + y'), c))) sprite

debreeAscii = [
    "+"
    ]        
        
projectileAscii = [
    "*"
    ]        
        
tankAsciiNorth Sprite.CaterpillarState1 = [
    " | ",
    "¤|¤",
    "+O+",
    "¤-¤",
    "   "]

tankAsciiNorth Sprite.CaterpillarState2 = [
    " | ",
    "+|+",
    "¤O¤",
    "+-+",
    "   "]

tankAsciiEast Sprite.CaterpillarState1 = [
    " ¤+¤ ",
    " |o==",
    " ¤+¤ "]

tankAsciiEast Sprite.CaterpillarState2 = [
    " +¤+ ",
    " |o==",
    " +¤+ "]

tankSprite North state = toSprite (tankAsciiNorth state)
tankSprite South state = toSprite (reverse (tankAsciiNorth state))
tankSprite West state = toSprite (map reverse (tankAsciiEast state))
tankSprite East state = toSprite (tankAsciiEast state)

toSprite :: [String] -> Color -> Sprite 
toSprite lines color = 
    Sprite 
        (width lines) 
        (width (transpose lines)) 
        (filter ((/= ' ') . fst . snd) (toSpriteLines 0 lines))
    where
        width lines = maximum (0 : map length lines)

        toSpriteLines :: Int -> [String] -> [((Int, Int), (Char, Color))]
        toSpriteLines row [] = []
        toSpriteLines row (line : lines) = toSpriteLine 0 row line ++ toSpriteLines (row + 1) lines

        toSpriteLine :: Int -> Int -> String -> [((Int, Int), (Char, Color))]
        toSpriteLine column row [] = []
        toSpriteLine column row (char : line) = ((column, row), (char, color)) : toSpriteLine (column + 1) row line

drawProjectile :: Picture s -> Color -> Vector -> ST s ()
drawProjectile picture playerColor location = 
    drawSprite picture location (toSprite projectileAscii playerColor)

drawTank :: Picture s -> Color -> (Vector, Direction) -> Sprite.CaterpillarState -> ST s ()
drawTank picture playerColor (location, direction) state =
    drawSprite picture location (tankSprite direction state playerColor)

drawWall :: Picture s -> Vector -> Vector -> ST s ()
drawWall picture position size = 
    let (width, height) = (round (vectorX size), round (vectorY size)) in
    let spriteLines = replicate height (replicate width '#') in
    let sprite = toSprite spriteLines White in
    drawSprite picture position sprite

drawDebree :: Picture s -> Vector -> ST s ()
drawDebree picture location = 
    drawSprite picture location (toSprite debreeAscii Yellow)

drawSprite :: Picture s -> Vector -> Sprite -> ST s ()
drawSprite picture location (Sprite width height points) = do
    ((x1, y1), (x2, y2)) <- getBounds picture
    let withinBounds (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2
    let (x, y) = toTuple location
    let points' = translatePoints (x - width `div` 2, y2 - y - height `div` 2) points
    forM_ (filter (withinBounds . fst) points') $ \(i, c) -> do
        writePixel picture i c
    
toTuple :: Vector -> (Int, Int)
toTuple vector = (round (vectorX vector), round (vectorY vector))

toVector :: (Int, Int) -> Vector
toVector (x, y) = (fromIntegral x, fromIntegral y)

