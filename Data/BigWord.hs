{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

---------------------------------------------------------
-- |
-- Module      : Data.BigWord
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : partable
--
-- Fixed-size bit vectors using type-level naturals.
---------------------------------------------------------

module Data.BigWord
    ( BigWord
    , (>+<)
    , takeBE
    , takeLE
    , giveBE
    , giveLE
    ) where

import Control.Monad
import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import Data.List
import Data.Proxy
import Data.Monoid
import Data.Word (Word8)
import GHC.TypeLits
import Text.Printf

-- | Type representing a sequence of n bits, or a non-negative integer smaller than 2^n
newtype BigWord (n :: Nat) = BigWord { getBigWord :: Integer }
    deriving (Eq, Ord, Real, Ix, PrintfArg, Data, Typeable)

instance KnownNat n => Read (BigWord n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (fromInteger a, str)) readsPrec

instance Show (BigWord n) where
    show = show . getBigWord

instance KnownNat n => Bounded (BigWord n) where
    minBound = 0
    maxBound = BigWord (bit (natValInt (Proxy :: Proxy n)) - 1)

-- Can't just derive because it need to get the default for bounded types
instance KnownNat n => Enum (BigWord n) where
    toEnum = BigWord . toEnum
    fromEnum = fromEnum . getBigWord

instance KnownNat n => Integral (BigWord n) where
    toInteger = getBigWord
    quotRem x y = case (quotRem `on` getBigWord) x y of (q, r) -> (BigWord q, BigWord r)

instance KnownNat n => Num (BigWord n) where
    fromInteger = BigWord . flip mod (bit (natValInt (Proxy :: Proxy n)))
    (+) = ((.).(.)) fromInteger ((+) `on` getBigWord)
    (*) = ((.).(.)) fromInteger ((*) `on` getBigWord)
    abs = id
    signum 0 = 0
    signum _ = 1
    negate = (+ 1) . complement

instance KnownNat n => Bits (BigWord n) where
        (.&.) = ((.).(.)) BigWord ((.&.) `on` getBigWord)
        (.|.) = ((.).(.)) BigWord ((.|.) `on` getBigWord)
        xor   = ((.).(.)) BigWord (xor   `on` getBigWord)
        complement = fromInteger . complement . getBigWord
        shift (BigWord x) i = BigWord $ shift x i `mod` bit (natValInt (Proxy :: Proxy n))
        rotate x i = let nat = natValInt (Proxy :: Proxy n)
                         dist = mod i nat
                     in shift x dist .|. shift x (nat - dist)
        bitSizeMaybe = Just . finiteBitSize
        bitSize = finiteBitSize
        isSigned = const False
        testBit = testBit . getBigWord
        bit i = if i < natValInt (Proxy :: Proxy n)
                then BigWord (bit i)
                else 0
        popCount = popCount . getBigWord

instance KnownNat n => FiniteBits (BigWord n) where
    finiteBitSize = const $ natValInt (Proxy :: Proxy n)

-- | Appends two BigWords, treating the second's bits as more significant.
(>+<) :: forall n m. (KnownNat n, KnownNat m, KnownNat (n + m)) => BigWord n -> BigWord m -> BigWord (n + m)
(BigWord x) >+< (BigWord y) = fromInteger $ x + shift y (natValInt (Proxy :: Proxy m))

-- | Transforms an action that results in an octet to one that results in a BigWord of the specified size.
-- Octets resulting from the input action occur in network-byte order.
-- If n is not a multiple of 8, leftover bits are truncated.
takeBE :: (Applicative f, KnownNat n) => f Word8 -> f (BigWord n)
takeBE = takeAux mapAccumL

-- | Transforms an action that results in an octet to one that results in a BigWord of the specified size.
-- Octets resulting from the input action occur in little-endian order.
-- If n is not a multiple of 8, leftover bits are truncated.
takeLE :: (Applicative f, KnownNat n) => f Word8 -> f (BigWord n)
takeLE = takeAux mapAccumR

takeAux :: forall f n. (Applicative f, KnownNat n) =>
    (  (Integer -> Word8 -> (Integer, Integer))
    -> Integer
    -> [Word8]
    -> (Integer, [Integer])
    ) -> f Word8 -> f (BigWord n)

takeAux f = fmap ( fromInteger
                 . getSum
                 . foldMap Sum
                 . snd
                 . f (\a b -> (a * 256, a * toInteger b)) 1
                 )
          . sequenceA
          . replicate (bytes (Proxy :: Proxy n))

-- | Break a BigWord into its constituent octets, and combine then as a monoid in network-byte order.
-- If n is not a multiple of 8, missing bits are treated as 0.
giveBE :: forall m n. (Monoid m, KnownNat n) => (Word8 -> m) -> BigWord n -> m
giveBE f = go (bytes (Proxy :: Proxy n)) . toInteger
  where
    go 0 _ = mempty
    go i n = f (fromInteger n) <> go (i - 1) (shiftR n 8)

-- | Break a BigWord into its constituent octets, and combine then as a monoid in little-endian order.
-- If n is not a multiple of 8, missing bits are treated as 0.
giveLE :: (Monoid m, KnownNat n) => (Word8 -> m) -> BigWord n -> m
giveLE = (.) getDual . giveBE . (.) Dual


bytes :: KnownNat n => Proxy n -> Int
bytes proxy = let (q, r) = natValInt proxy `quotRem` 8
              in case r of 0 -> q
                           _ -> q + 1

-- Just to make things cleaner:
natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

