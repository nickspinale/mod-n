{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

---------------------------------------------------------
-- |
-- Module      : Data.W
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

import Control.Applicative
import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import Data.Proxy
import Data.Monoid
import Data.Traversable
import Data.Type.Equality
import GHC.TypeLits
import Text.Printf

-- | Type representing an equivalence class under the integers mod n
newtype Mod (n :: Nat) = Mod { unMod :: Integer }
    deriving (Eq, Ord, Real, Ix, PrintfArg, Data, Typeable)

-- Original name was BigWord, but since using this module requires more
-- explicit type signatures, I decided to use just W. This may be stupid.
-- Sorry if the name conflicts with your code.

-------------------------------
-- INSTANCES
-------------------------------

instance KnownNat n => Read (Mod n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (fromInteger a, str)) readsPrec

instance Show (Mod n) where
    show = show . unMod

instance KnownNat n => Bounded (Mod n) where
    minBound = 0
    maxBound = Mod (bit (natValInt (Proxy :: Proxy n)) - 1)

-- Can't just derive because it need to get the default for bounded types
instance KnownNat n => Enum (Mod n) where
    toEnum = Mod . toEnum
    fromEnum = fromEnum . unMod

instance KnownNat n => Integral (Mod n) where
    toInteger = unMod
    quotRem x y = case (quotRem `on` unMod) x y of (q, r) -> (Mod q, Mod r)

instance KnownNat n => Num (Mod n) where
    fromInteger = Mod . flip mod (bit (natValInt (Proxy :: Proxy n)))
    (+) = ((.).(.)) fromInteger ((+) `on` unMod)
    (*) = ((.).(.)) fromInteger ((*) `on` unMod)
    abs = id
    signum 0 = 0
    signum _ = 1
    negate = fromInteger . negate . toInteger

-------------------------------
-- HELPERS
-------------------------------

natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

