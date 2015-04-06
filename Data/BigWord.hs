{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , accumulate
    , accumulate'
    , chunks
    , chunks'

    ) where

import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import Data.Proxy
import Data.Monoid
import Data.Traversable
import GHC.TypeLits
import Text.Printf

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

-------------------------------
-- EXPORTS
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

-- | Transforms an applicative action that results in a @'W' n@ to one that results in a @'W' m@.
--
-- @'W' n@'s are accumulated with significance increasing from left to right.
--
-- If @n@ does not divide @m@, leftover bits are truncated.
--
-- Example usage, parsing a little endian 160-bit word:
--
-- >    import Data.Attoparsec.ByteString
-- >
-- >    parseW160 :: Parser (W 610)
-- >    parseW160 = accumulate $ (fromIntegral :: Word8 -> W 8) <$> anyWord8
accumulate :: (Applicative f, KnownNat m, KnownNat n) => f (W n) -> f (W m)
accumulate = takeAux mapAccumR

-- | Same as @'accumulate'@, but gathers in the opposite order
accumulate' :: (Applicative f, KnownNat m, KnownNat n) => f (W n) -> f (W m)
accumulate' = takeAux mapAccumL

-- | Break a @'W' m@ into its constituent @'W' n@'s, and combine using the supplied monoid
--
-- Chunks are appended with significance increasing from left to right.
--
-- If @n@ does not divide @m@, missing bits are set to 0.
--
-- Example usage, building a little endian 160-bit word:
--
-- >    import Data.ByteString.Builder
-- >
-- >    buildW160 :: W 160 -> Builder
-- >    buildW160 = chunks $ word8 . (fromIntegral :: W 8 -> Word8)
chunks :: forall a m n. (Monoid a, KnownNat m, KnownNat n) => (W n -> a) -> W m -> a
chunks = (.) getDual . chunks' . (.) Dual

-- | Same as @'chunks'@, but appends in opposite order
chunks' :: forall a m n. (Monoid a, KnownNat m, KnownNat n) => (W n -> a) -> W m -> a
chunks' f = go (quot' m n) . toInteger
  where
    go 0 _ = mempty
    go i x = f (fromInteger x) <> go (i - 1) (shiftR x n)
    m = natValInt (Proxy :: Proxy m)
    n = natValInt (Proxy :: Proxy n)

-------------------------------
-- HELPERS
-------------------------------

takeAux :: forall f n m. (Applicative f, KnownNat m, KnownNat n) =>
    (  (Integer -> W n -> (Integer, Integer))
    -> Integer
    -> [W n]
    -> (Integer, [Integer])
    ) -> f (W n) -> f (W m)

takeAux f = fmap ( fromInteger
                 . getSum
                 . foldMap Sum
                 . snd
                 . f (\a b -> (a * 2^n, a * toInteger b)) 1
                 )
          . sequenceA
          . replicate (quot' m n)
  where
    m = natValInt (Proxy :: Proxy m)
    n = natValInt (Proxy :: Proxy n)


quot' :: Int -> Int -> Int
quot' m n = let (q, r) = m `quotRem` n
              in case r of 0 -> q
                           _ -> q + 1

natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

