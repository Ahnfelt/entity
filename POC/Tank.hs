module Tank where

import Game
import qualified Position as Position

new position' = object $ \this -> do
    position <- Position.new this position' (0, 0)
    return $ position .:. Nil

