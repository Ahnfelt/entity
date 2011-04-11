module AsciiShooter.Main where

import Feature
import qualified AsciiShooter.Entity.Player as Player
import qualified AsciiShooter.Feature.Position as Position
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

run n = do
    let state = GameState {}
    p <- runGame state Player.new
    replicateM_ n $ do 
        runGame state $ updateEntity p
        let Just pos = getFeature p
        pos' <- runGame state $ get Position.position pos
        print pos'


