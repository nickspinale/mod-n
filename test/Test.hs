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

module Test where

import Data.BigWord

import Control.Applicative
import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import Data.Proxy
import Data.Monoid
import Data.Traversable
import GHC.TypeLits
import Text.Printf
import System.Random hiding (split)

import Data.Functor.Identity

x = 83 :: W 8
