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
    , assembleL
    , assembleR
    , disassembleL
    , disassembleR

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

(>+<) :: forall n m o. ( KnownNat m
                       , KnownNat n
                       , KnownNat o
                       , o ~ (m + n)
                       , o ~ (n + m)
                       ) => W m -> W n -> W o

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

split :: forall n m o. ( KnownNat m
                       , KnownNat n
                       , KnownNat o
                       , o ~ (m + n)
                       , o ~ (n + m)
                       ) => W o -> (W m, W n)

split (W z) = (fromInteger z, fromInteger $ shiftR z (natValInt (Proxy :: Proxy m)))

-------------------------------
-- UTILITY FUNCTIONS
-------------------------------

-- | Transforms an applicative action that results in a @'W' d@ to on that results in a @'W' n@, provided that @d|n@ (hence the @:|:@ constraint), treating the first results as less significant.
--
-- Example using attoparsec to parse a little-endian unsigned 128-bit integer:
--
-- >    import Data.Attoparsec.ByteString
-- >    import Data.Word
-- >
-- >    anyWord128LE :: Parser (W 128)
-- >    anyWord128LE = assembleL $ fmap (fromIntegral :: Word8 -> W 8) anyWord8

assembleL :: (Applicative f, d :|: n) => f (W d) -> f (W n)
assembleL = assemble (>+<)

-- | Same as assembleL, but treats the first results a more significant>
--
-- Here's the example above, modified to parse in network-byte order:
--
-- >    anyWord128BE :: Parser (W 128)
-- >    anyWord128B  = assembleR $ fmap (fromIntegral :: Word8 -> W 8) anyWord8

assembleR :: (Applicative f, d :|: n) => f (W d) -> f (W n)
assembleR = assemble (flip (>+<))

-- | Breaks a @'W' n@ into its constituent @d@-sized chunks, and combines them according to the provided monoid.
--
-- Example using a bytestrings
--
-- >    import Data.ByteString.Builder
-- >
-- >    word128LE :: W 128 -> Builder
-- >    word128LE = disassembleL (word8 . (fromIntegral :: W 8 -> Word8))

disassembleL :: (Monoid m , d :|: n) => (W d -> m) -> (W n -> m)
disassembleL = disassemble

-- | Same as disassembleL, but @mappend@s from right to left.
--
-- @disassembleL@'s example adjusted to build in network-byte order:
--
-- >    import Data.ByteString.Builder
-- >
-- >    word128BE :: W 128 -> Builder
-- >    word128BE = disassembleR (word8 . (fromIntegral :: W 8 -> Word8))

disassembleR :: (Monoid m , d :|: n) => (W d -> m) -> (W n -> m)
disassembleR f = getDual . disassemble (Dual . f)

-------------------------------
-- :|:
-------------------------------

class (KnownNat d, KnownNat n) => d :|: n where

    assemble :: forall f. Applicative f
             => ( forall a b c. ( KnownNat a
                                , KnownNat b
                                , KnownNat c
                                , c ~ (a + b)
                                , c ~ (b + a)
                                )
                => W a -> W b -> W c
                )
             -> f (W d)
             -> f (W n)

    disassemble :: forall m. Monoid m => (W d -> m) -> (W n -> m)

instance KnownNat n => n :|: n where
    assemble _ = id
    disassemble = id

instance {-# OVERLAPPABLE #-} ( KnownNat n
                              , d :|: n'
                              , n ~ (d + n')
                              , n ~ (n' + d)
                              ) => d :|: n where

    assemble c f = liftA2 c f (assemble c f)

    disassemble f w = f l <> disassemble f r
      where
        l :: W d
        r :: W n'
        (l, r) = split w

-------------------------------
-- HELPERS
-------------------------------

natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

