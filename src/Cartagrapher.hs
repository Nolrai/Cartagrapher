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
import Data.Vector.Unboxed.Sized as VU
    ( MVector, Unbox, replicateM, Vector )
import Data.Vector.Unboxed.Mutable.Sized as VM ( read, write )
import qualified Data.List as L
import Test.SmallCheck.Series ( Serial(..), (\/) )
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Data.Finite ( Finite, finites ) 

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
    ab = L.nub [a, b] -- its okay for a == b
    cd = case action of
      Toggle c -> [c]
      Swap c d -> [c, d]
    indexes = (L.++) ab cd

pairwiseDistinct :: Ord a => [a] -> Bool
pairwiseDistinct [] = True
pairwiseDistinct (x:xs) = L.notElem x xs && pairwiseDistinct xs

type MyConstraint (n :: Nat) m = (KnownNat n, MonadReader (VU.MVector n (PrimState m) Bool) m , PrimMonad m)

runInstruction :: MyConstraint n m => Instruction n -> m ()
runInstruction Instruction {..} =
  do
    let Pair (a,b) = control
    a' <- (== fst a) <$> readV (snd a)
    b' <- (== fst b) <$> readV (snd b)
    when (a' && b') $
      case action of
        Toggle i -> toggleM i
        Swap i j -> swapM i j

readV :: MyConstraint n m => Finite n -> m Bool
readV (i :: (Finite n)) = do
  v <- ask
  VM.read v i

writeV :: MyConstraint n m => Finite n -> Bool -> m ()
writeV (i :: (Finite n)) (x :: Bool) = do
  v <- ask
  VM.write v i x

toggleM :: MyConstraint n m => Finite n -> m ()
toggleM i = do
  b <- readV i
  writeV i (not b)

swapM :: MyConstraint n m => Finite n -> Finite n -> m ()
swapM i j = do
  ixValue <- readV i
  jxValue <- readV j
  writeV i jxValue
  writeV j ixValue

instance (KnownNat n, Unbox a, Serial m a) => Serial m (VU.Vector n a) where
  series = VU.replicateM series

instance (Serial m Int, KnownNat n) => Serial m (Finite n) where
  series = go finites
    where
      go [] = mzero
      go (x:xs) = pure x \/ go xs