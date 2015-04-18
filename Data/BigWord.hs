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
    , takeBE
    , takeLE
    , giveBE
    , giveLE
    ) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
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

-- For casting (between BigWord types), use fromIntegral
newtype BigWord (n :: Nat) = BigWord { getBigWord :: Integer }
    deriving (Eq, Data, Ord, Real, Ix, PrintfArg)

instance KnownNat n => Read (BigWord n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (fromInteger a, str)) readsPrec

instance Show (BigWord n) where
    show = show . getBigWord

(>+<) :: forall n m. (KnownNat n, KnownNat m, KnownNat (n + m)) => BigWord n -> BigWord m -> BigWord (n + m)
(BigWord x) >+< (BigWord y) = fromInteger $ x + shift y (natValInt (Proxy :: Proxy m))

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

takeBE :: (Applicative f, KnownNat n) => f Word8 -> f (BigWord n)
takeBE = takeAux mapAccumL

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

giveBE :: forall m n. (Monoid m, KnownNat n) => (Word8 -> m) -> BigWord n -> m
giveBE f = foldMap f . evalState (replicateM (bytes (Proxy :: Proxy n)) (state $ \n -> (fromIntegral n, shiftL n 8)))

giveLE :: (Monoid m, KnownNat n) => (Word8 -> m) -> BigWord n -> m
giveLE = (.) getDual . giveBE . (.) Dual


bytes :: KnownNat n => Proxy n -> Int
bytes proxy = let (q, r) = natValInt proxy `quotRem` 8
              in case r of 0 -> q
                           _ -> q + 1

-- Just to make things cleaner:
natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

