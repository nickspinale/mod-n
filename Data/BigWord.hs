{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

---------------------------------------------------------
-- |
-- Module      : Data.W
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : partable
--
-- Fixed size bit vectors using type-level naturals.
---------------------------------------------------------

module Data.BigWord
    (
    -- * The 'W' newtype
      W
    -- * Operations
    , (>+<)
    , split

    -- * Utility functions
    , (:|:)(..)

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
import System.Random hiding (split)

-- | Type representing a sequence of @n@ bits, or a non-negative integer smaller than @2^n@.
newtype W (n :: Nat) = W { getW :: Integer }
    deriving (Eq, Ord, Real, Ix, PrintfArg, Data, Typeable)

-- Original name was BigWord, but since using this module requires more
-- explicit type signatures, I decided to use just W. This may be stupid.
-- Sorry if the name conflicts with your code.

-------------------------------
-- INSTANCES
-------------------------------

instance KnownNat n => Read (W n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (fromInteger a, str)) readsPrec

instance Show (W n) where
    show = show . getW

instance KnownNat n => Bounded (W n) where
    minBound = 0
    maxBound = W (bit (natValInt (Proxy :: Proxy n)) - 1)

-- Can't just derive because it need to get the default for bounded types
instance KnownNat n => Enum (W n) where
    toEnum = W . toEnum
    fromEnum = fromEnum . getW

instance KnownNat n => Integral (W n) where
    toInteger = getW
    quotRem x y = case (quotRem `on` getW) x y of (q, r) -> (W q, W r)

instance KnownNat n => Num (W n) where
    fromInteger = W . flip mod (bit (natValInt (Proxy :: Proxy n)))
    (+) = ((.).(.)) fromInteger ((+) `on` getW)
    (*) = ((.).(.)) fromInteger ((*) `on` getW)
    abs = id
    signum 0 = 0
    signum _ = 1
    negate = (+ 1) . complement

instance KnownNat n => Bits (W n) where
        (.&.) = ((.).(.)) W ((.&.) `on` getW)
        (.|.) = ((.).(.)) W ((.|.) `on` getW)
        xor   = ((.).(.)) W (xor   `on` getW)
        complement = fromInteger . complement . getW
        shift (W x) i = W $ shift x i `mod` bit (natValInt (Proxy :: Proxy n))
        rotate x i = let nat = natValInt (Proxy :: Proxy n)
                         dist = mod i nat
                     in shift x dist .|. shift x (nat - dist)
        bitSizeMaybe = Just . finiteBitSize
        bitSize = finiteBitSize
        isSigned = const False
        testBit = testBit . getW
        bit i = if i < natValInt (Proxy :: Proxy n)
                then W (bit i)
                else 0
        popCount = popCount . getW

instance KnownNat n => FiniteBits (W n) where
    finiteBitSize = const $ natValInt (Proxy :: Proxy n)

instance KnownNat n => Random (W n) where
    randomR (a, b) g = (fromInteger r, g')
      where (r, g') = randomR (toInteger a, toInteger b) g
    random = randomR (minBound, maxBound)

-------------------------------
-- OPERATIONS
-------------------------------

-- | Appends two @'W'@'s, treating the second's bits as more significant.
--
-- Example usage:
--
-- >    import Network.Socket
-- >
-- >    fromHostAddress6 :: HostAddress6 -> W 128
-- >    fromHostAddress6 (a, b, c, d) = f a >+< f b >+< f c >+< f d
-- >      where
-- >        f = fromIntegral :: Word32 -> W 32
(>+<) :: forall n m. (KnownNat m, KnownNat n, KnownNat (m + n)) => W m -> W n -> W (m + n)
(W x) >+< (W y) = fromInteger $ x + shift y (natValInt (Proxy :: Proxy m))

-- | The inverse of @'>+<'@
--
-- >    forall a b. split (a >+< b) == (a, b)
--
-- Example usage:
--
-- >    import Network.Socket
-- >
-- >    toHostAddress6 :: W 128 -> HostAddress6
-- >    toHostAddress6 w =  (f a, f b, f c, f d)
-- >      where
-- >        f = fromIntegral :: W 32 -> Word32
-- >        (a, x) = split w
-- >        (b, y) = split x
-- >        (c, d) = split y
split :: forall n m. (KnownNat m, KnownNat n, KnownNat (m + n)) => W (m + n) -> (W m, W n)
split (W z) = (fromInteger z, fromInteger $ shiftR z (natValInt (Proxy :: Proxy m)))

-------------------------------
-- :|:
-------------------------------

class (KnownNat d, KnownNat n) => d :|: n where
    assemble :: forall f. Applicative f
             => ( forall a b. (KnownNat a, KnownNat b, KnownNat (a + b))
                => W a
                -> W b
                -> W (a + b)
                )
             -> f (W d)
             -> f (W n)

instance KnownNat n => n :|: n where
    assemble c f = f

instance (KnownNat n, d :|: n', (d + n') ~ n) => d :|: n where
    assemble c f = liftA2 c f (assemble c f)

-------------------------------
-- HELPERS
-------------------------------

natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

