{-# Language DerivingVia, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables, ConstraintKinds, DataKinds, KindSignatures, GADTs, TypeOperators, NoStarIsType #-}
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
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Sized as VU
import qualified Data.Vector.Storable.Mutable.Sized as VM ( read, write )
import qualified Data.List as L
import Test.SmallCheck.Series ( Serial(..), (\/) )
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Data.Finite
import GHC.TypeLits
import Data.ByteString
import Data.Bits
import Foreign.Ptr (castPtr)

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

hammingWeight :: VU.Vector n Bool -> Natural 
hammingWeight = fromIntegral . VU.sum . VU.map (\ b -> if b then 1 :: Int else 0 :: Int)

instance (KnownNat n, V.Storable a, Serial m a) => Serial m (VU.Vector n a) where
  series = VU.replicateM series

instance (Serial m Int, KnownNat n) => Serial m (Finite n) where
  series = go finites
    where
      go [] = mzero
      go (x:xs) = pure x \/ go xs

packFunction :: (KnownNat n, KnownNat (2 ^ n), KnownNat ((2 ^ n) * n)) =>
 (VU.Vector n Bool -> VU.Vector n Bool) -> IO ByteString 
packFunction = packVector . flattenVector . functionToVector

packVector :: KnownNat n =>
  VU.Vector n Bool -> IO ByteString
packVector v =
  let len = VU.length v
      size = len `div` 8 + (if len `mod` 8 == 0 then 0 else 1)
      in
  V.unsafeWith (VU.fromSized v) $ \ptr ->
    packCStringLen (castPtr ptr, size)

flattenVector ::  (KnownNat n, KnownNat (2 ^ n), KnownNat ((2 ^ n) * n)) => 
  VU.Vector (2 ^ n) (VU.Vector n Bool) -> VU.Vector ((2 ^ n) * n) Bool
flattenVector v =
  VU.generate $ \ i ->
    let (j, k) = separateProduct i in
      VU.index (VU.index v j) k

functionToVector :: (KnownNat n, KnownNat (2 ^ n)) => 
  (VU.Vector n Bool -> VU.Vector n Bool) -> VU.Vector (2 ^ n) (VU.Vector n Bool)
functionToVector f = 
  VU.generate (f . toBits . fromIntegral)

toBits :: KnownNat n => Word32 -> VU.Vector n Bool
toBits n = VU.generate ((n `testBit`) . fromIntegral)