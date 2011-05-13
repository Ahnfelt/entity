module AsciiShooter.Utilities.Mechanics where

import Prelude hiding ((/), acos, sqrt)
import AsciiShooter.Utilities.Floating
    
type Vector = (Double, Double)
type LineSegment = (Vector, Vector)
type Box = (Vector, Vector)
type Position = Vector
type Velocity = Vector
type Acceleration = Vector
type Dimension = Vector
type Angle = Double
type Magnitude = Double
type Duration = Double


-- Vector from angle and magnitude
vector :: Angle -> Magnitude -> Vector
vector a m = (cos a * m, sin a * m)


-- Approximate an angle a' from a, taking 1/d iterations to turn a full circle
approximateAngle :: Duration -> Angle -> Angle -> Angle
approximateAngle t a a' = 
    let a'' = atan2 (sin (a' - a)) (cos (a' - a)) in
    if t > abs a'' then a'
    else a + signum a'' * t

-- Interpolates linearly between a series of values (elapsed, value, [(time, value)])
-- For easiest usage, let 0 <= t <= 1. Durations cannot be negative.
interpolate :: Duration -> Double -> [(Duration, Double)] -> Double
interpolate t i _ | t <= 0 = i
interpolate t i ((d, v):ps) | t > d = interpolate (t - d) v ps
interpolate t i ((d, v):ps) = let f = t / (d + epsilon) in (1.0 - f) * i + f * v
interpolate t i [] = i


epsilon = 0.00001

--- Vector stuff
-----------------------

vectorX :: Vector -> Double
vectorX = fst

vectorY :: Vector -> Double
vectorY = snd

zero :: Vector
zero = (0, 0)

-- Operators for 2d vectors

(.+.) :: Vector -> Vector -> Vector
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

(.-.) :: Vector -> Vector -> Vector
(a, b) .-. (c, d) = (a - c, b - d)

(.*) :: Vector -> Magnitude -> Vector
(x, y) .* s = (x * s, y * s)

(./) :: Vector -> Double -> Vector
(a, b) ./ d = (a/d, b/d)

(*.) :: Magnitude -> Vector -> Vector
s *. (x, y) = (s * x, s * y)

(/.) :: Double -> Vector -> Vector
d /. (a, b) = (d/a, d/b)

infixl 6 .+.
infixl 6 .-.
infixr 7 .*
infixr 7 ./
infixr 7 *.
infixr 7 /.

vectorLength :: Vector -> Double
vectorLength (a,b) = sqrt (a*a + b*b)

norm :: Vector -> Vector
norm v = v ./ vectorLength v

dot :: Vector -> Vector -> Double
dot (a, b) (c, d) = a*c + b*d

angle :: Vector -> Vector -> Double
angle v1 v2 = let x = norm v1 `dot` norm v2 in if x < -1 || x > 1 then pi else acos x


