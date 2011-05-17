module AsciiShooter.Entity.Projectile (new) where

import Feature
import AsciiShooter.Player
import AsciiShooter.Sprite
import AsciiShooter.Utilities.Mechanics
import AsciiShooter.Feature.Physics (Hit (..))
import qualified AsciiShooter.Feature.Direction as Direction
import qualified AsciiShooter.Feature.Physics as Physics
import qualified AsciiShooter.Feature.Listener as Listener
import qualified AsciiShooter.Feature.Animation as Animation
import qualified AsciiShooter.Feature.Damage as Damage
import qualified AsciiShooter.Feature.Random as Random
import qualified AsciiShooter.Entity.Debree as Debree

import Control.Monad

new :: Player -> Position -> Velocity -> Int -> Game (Entity ())
new player position velocity bounces = object $ \this key -> do
    physics <- Physics.new position velocity zero (0.5, 0.5) False True key
    hitListener <- Listener.new (method (onHit bounces) this)
    animation <- Animation.new physics (Projectile player)
    random <- Random.new
    damage <- Damage.new 10
    return $ toEntity $ physics .:. hitListener .:. animation .:. damage .:. random .:. nil


onHit bounces this Hit { hitEntity = entity, hitMyFault = myFault, hitBounce = bounce } = do
    case getFeature entity of
        Just physics | Physics.getCanBlock physics || Physics.getCanBeBlocked physics -> do
            let random = requireFeature this
            position <- Physics.getPosition (requireFeature this)
            replicateM 10 $ do
                a <- Random.uniform (0, 2 * pi) random
                m <- Random.uniform (0, 20) random
                t <- Random.uniform (0.2, 1) random
                debree <- Debree.new position (vector a m) 5 t
                spawn debree
            unspawn this
            case bounce of
                Just bounce | myFault && bounces > 0 -> do
                    position <- Physics.getPosition (requireFeature this)
                    let (a, m) = (angle bounce, magnitude bounce * 0.60)
                    let (a1, a2) = (a - pi / 9, a + pi / 9)
                    let (p1, v1) = (position .+. vector a1 1, vector a1 m)
                    let (p2, v2) = (position .+. vector a2 1, vector a2 m)
                    projectile1 <- new 4 p1 v1 (bounces - 1)
                    spawn projectile1
                    projectile2 <- new 4 p2 v2 (bounces - 1)
                    spawn projectile2
                    return ()
                _ -> return ()
        _ -> return ()

