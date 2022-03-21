{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Test.Instances.Finite where

import Test.SmallCheck.Series
import Data.Finite
import GHC.TypeNats
import Relude

instance (Monad m, KnownNat n) => Serial m (Finite n) where
  series = go finites
    where
      go [] = mzero
      go (x:xs) = pure x \/ go xs

