{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Instances.Finite where

import Test.SmallCheck.Series
import Data.Finite
import GHC.TypeNats

instance (Serial m Int, KnownNat n) => Serial m (Finite n) where
  series = go finites
    where
      go [] = mzero
      go (x:xs) = pure x \/ go xs

