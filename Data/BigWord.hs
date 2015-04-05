{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.BigWord
    ( BigWord
    ) where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Function
import Data.Proxy
import GHC.TypeLits
import Text.Printf

-- Integer seems more trustworthy than numeric.natural(base depends on it...),
-- plus negative values make wrapping easier.
newtype BigWord (n :: Nat) = BigWord { unwrap :: Integer }
    deriving (Eq, Data, Ord, Read, Real, Show, Ix, PrintfArg)

instance KnownNat n => Enum (BigWord n) where
instance KnownNat n => Integral (BigWord n) where
instance KnownNat n => Num (BigWord n) where

instance KnownNat n => Bits (BigWord n) where
        (.&.) = ((.).(.)) BigWord ((.&.) `on` unwrap)
        (.|.) = ((.).(.)) BigWord ((.|.) `on` unwrap)
        xor   = ((.).(.)) BigWord (xor   `on` unwrap)
        bitSizeMaybe = Just . finiteBitSize
        bitSize = finiteBitSize
        isSigned = const False
        testBit = testBit . unwrap

instance KnownNat n => FiniteBits (BigWord n) where
    finiteBitSize = const . fromInteger $ natVal (Proxy :: Proxy n)
