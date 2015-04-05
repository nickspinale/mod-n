{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

import Data.BigWord
import Data.Word
import Data.Bits

i = 5430925843025432 :: Integer

w = fromInteger i :: Word32

b = fromInteger i :: BigWord 32

prnt :: Bits a => a -> [Int]
prnt x = map (fromEnum . testBit x) [31,30..0]
