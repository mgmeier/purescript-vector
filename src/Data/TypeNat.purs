-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeNat
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
--
-- | Simple type level naturals for vector and matrix sizes
--
-----------------------------------------------------------------------------

module Data.TypeNat where

import Prelude
import Data.Generic (anyProxy, Proxy())

data Zero
data Suc a

type One   = Suc Zero
type Two   = Suc (Suc Zero)
type Three = Suc (Suc (Suc Zero))
type Four  = Suc (Suc (Suc (Suc Zero)))

class Sized a where
  sized :: Proxy a -> Int

instance sz :: Sized Zero where
  sized _ = 0
instance ss :: (Sized a) => Sized (Suc a) where
  sized _ = 1 + sized (anyProxy :: Proxy a)
