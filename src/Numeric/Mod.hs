{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

---------------------------------------------------------
-- |
-- Module      : Numeric.Mod
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Integers under a modulus
---------------------------------------------------------

module Numeric.Mod
    (
    -- * The 'Mod' newtype
      Mod
    ) where

import Data.Data
import Data.Function
import Data.Ix
import GHC.Exts
import GHC.TypeLits
import Text.Printf

-- | Type representing an equivalence class under the integers modulo n
newtype Mod (n :: Nat) = Mod { integer :: Integer }
    deriving (Eq, Enum, Integral, Ord, Real, Ix, PrintfArg, Data, Typeable)

-------------------------------
-- INSTANCES
-------------------------------

instance Show (Mod n) where
    show = show . integer

instance Read (Mod n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (Mod a, str)) readsPrec

instance KnownNat n => Bounded (Mod n) where
    minBound = 0
    maxBound = Mod (natVal' (proxy# :: Proxy# n) - 1)

instance KnownNat n => Num (Mod n) where
    fromInteger = Mod . flip mod (natVal' (proxy# :: Proxy# n))
    (+) = ((.).(.)) fromInteger ((+) `on` integer)
    (*) = ((.).(.)) fromInteger ((*) `on` integer)
    abs = id
    signum 0 = 0
    signum _ = 1
    negate = fromInteger . negate . integer
