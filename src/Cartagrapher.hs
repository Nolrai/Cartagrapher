{-# Language DerivingVia, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables, ConstraintKinds, DataKinds, KindSignatures, GADTs #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright: (c) 2021 Chris Upshaw
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>

cartisian genetic programing
-}

module Cartagrapher where

import Relude
import qualified Data.Vector.Sized as VS
import Data.Vector.Unboxed.Sized as VU
import Data.Vector.Unboxed.Mutable.Sized as VM
import qualified Data.List as L
import Test.SmallCheck.Series
import Control.Monad.Primitive
import Data.Finite 

data Action n = Toggle (Finite n) | Swap (Finite n) (Finite n)
  deriving stock (Show, Read, Eq, Ord, Generic)

instance (Monad m, Serial m (Finite n)) => Serial m (Action n)

newtype Pair a = Pair {toTuple :: (a, a)}
  deriving stock (Functor)
  deriving newtype (Ord, Eq, Show, Read, Generic)

instance Serial m a => Serial m (Pair a)

data Instruction n = Instruction {control :: Pair (Bool, Finite n), action :: Action n}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance (Monad m, Serial m (Finite n)) => Serial m (Instruction n)

isValid :: Instruction n -> Bool
isValid Instruction {..} = 
  pairwiseDistinct indexes
  where
    Pair ((_, a), (_, b)) = control
    cd = case action of
      Toggle c -> [c]
      Swap c d -> [c, d]
    indexes = a:b:cd

pairwiseDistinct :: Ord a => [a] -> Bool
pairwiseDistinct [] = True
pairwiseDistinct (x:xs) = Relude.notElem x xs && pairwiseDistinct xs

type MyConstraint (n :: Nat) m = (KnownNat n, MonadReader (VU.MVector n (PrimState m) Bool) m , PrimMonad m)

runInstruction :: MyConstraint n m => Instruction n -> m ()
runInstruction Instruction {..} =
  do
    let Pair (a,b) = control
    a' <- (== fst a) <$> readV (snd a)
    b' <- (== fst b) <$> readV (snd b)
    when (a' && b') $
      case action of
        Toggle ix -> toggleM ix
        Swap ix jx -> swapM ix jx

readV :: MyConstraint n m => Finite n -> m Bool
readV (ix :: (Finite n)) = do
  v <- ask
  VM.read v (fromIntegral ix)

writeV :: MyConstraint n m => Finite n -> Bool -> m ()
writeV (ix :: (Finite n)) (x :: Bool) = do
  v <- ask
  VM.write v (fromIntegral ix) x

toggleM :: MyConstraint n m => Finite n -> m ()
toggleM ix = do
  b <- readV ix
  writeV ix (not b)

swapM :: MyConstraint n m => Finite n -> Finite n -> m ()
swapM ix jx = do
  ixValue <- readV ix
  jxValue <- readV jx
  writeV ix jxValue
  writeV jx ixValue

instance (KnownNat n, Serial m a) => Serial m (VS.Vector n a) where
  series = VS.replicateM series

instance (Serial m Int, KnownNat n) => Serial m (Finite n) where
  series = go finites
    where
      go [] = mzero
      go (x:xs) = pure x \/ go xs