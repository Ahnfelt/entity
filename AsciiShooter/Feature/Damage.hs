{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module AsciiShooter.Feature.Damage where

import Data.Typeable
import Data.Record.Label
import Feature
import qualified AsciiShooter.Feature.Health as Health

data Type = Type {
    _damage :: Var Double
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Double -> Game Type
new damage = do
    return Type .$. damage

doDamage :: Health.Type -> Type -> Game Double
doDamage health self = do
    amount <- get damage self
    Health.add (-amount) health

