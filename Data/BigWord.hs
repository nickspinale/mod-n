{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.BigWord
    ( BigWord
    , (>+<)
    ) where

import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import Data.Proxy
import GHC.TypeLits
import Text.Printf

-- Just to make things cleaner:
natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

-- Integer seems more trustworthy than numeric.natural(base depends on it...),
-- plus negative values make wrapping easier.
-- For casting (between BigWord types), use fromIntegral
newtype BigWord (n :: Nat) = BigWord { unwrap :: Integer }
    deriving (Eq, Data, Ord, Read, Real, Show, Ix, PrintfArg)

(>+<) :: forall n m. (KnownNat n, KnownNat m, KnownNat (n + m)) => BigWord n -> BigWord m -> BigWord (n + m)
(BigWord x) >+< (BigWord y) = fromInteger $ x + shift y (natValInt (Proxy :: Proxy m))

instance KnownNat n => Bounded (BigWord n) where
    minBound = 0
    maxBound = bit (natValInt (Proxy :: Proxy n)) - 1

-- Can't just derive because it need to get the default for bounded types
instance KnownNat n => Enum (BigWord n) where
    toEnum = BigWord . toEnum
    fromEnum = fromEnum . unwrap

instance KnownNat n => Integral (BigWord n) where
    toInteger = unwrap
    quotRem x y = case (quotRem `on` unwrap) x y of (q, r) -> (BigWord q, BigWord r)

instance KnownNat n => Num (BigWord n) where
    fromInteger = BigWord . flip mod (bit (natValInt (Proxy :: Proxy n)))
    (+) = ((.).(.)) fromInteger ((+) `on` unwrap)
    (*) = ((.).(.)) fromInteger ((*) `on` unwrap)
    abs = id
    signum 0 = 0
    signum _ = 1
    negate = (+ 1) . complement

instance KnownNat n => Bits (BigWord n) where
        (.&.) = ((.).(.)) BigWord ((.&.) `on` unwrap)
        (.|.) = ((.).(.)) BigWord ((.|.) `on` unwrap)
        xor   = ((.).(.)) BigWord (xor   `on` unwrap)
        complement x = maxBound - x
        shift (BigWord x) i = BigWord $ shift x i `mod` bit (natValInt (Proxy :: Proxy n))
        rotate x i = let nat = natValInt (Proxy :: Proxy n)
                         dist = mod i nat
                     in shift x dist .|. shift x (nat - dist)
        bitSizeMaybe = Just . finiteBitSize
        bitSize = finiteBitSize
        isSigned = const False
        testBit = testBit . unwrap
        bit i = if i < natValInt (Proxy :: Proxy n)
                then BigWord (bit i)
                else 0
        popCount = popCount . unwrap

instance KnownNat n => FiniteBits (BigWord n) where
    finiteBitSize = const $ natValInt (Proxy :: Proxy n)
