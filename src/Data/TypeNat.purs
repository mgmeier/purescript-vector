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
import Type.Proxy (Proxy(..))

data Zero
data One
data Two
data Three
data Four

class Sized a where
  sized :: Proxy a -> Int

instance sz :: Sized Zero where
  sized _ = 0
instance s1 :: Sized One where
  sized _ = 1
instance s2 :: Sized Two where
  sized _ = 2
instance s3 :: Sized Three where
  sized _ = 3
instance s4 :: Sized Four where
  sized _ = 4
  
