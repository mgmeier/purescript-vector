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

import Type.Proxy (Proxy())

data Zero
data One
data Two
data Three
data Four

class Sized :: Type -> Constraint
class Sized a where
  sized :: Proxy a -> Int

instance Sized Zero where
  sized _ = 0
instance Sized One where
  sized _ = 1
instance Sized Two where
  sized _ = 2
instance Sized Three where
  sized _ = 3
instance Sized Four where
  sized _ = 4