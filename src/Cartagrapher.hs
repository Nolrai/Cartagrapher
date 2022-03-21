{-# Language DerivingVia, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# Language NoImplicitPrelude #-}

{- |
Copyright: (c) 2021 Chris Upshaw
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>

cartisian genetic programing
-}

module Cartagrapher where

import Relude
import qualified Data.List as L
import Test.SmallCheck.Series
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Finite ( Finite, finites )
import GHC.TypeLits ()
import Data.ByteString ()
import Data.Bits ()
import Data.BitsN
    ( clearBit', complementBit', setBit', testBit', BitsN )
import Data.ByteString.Builder
import Data.Vector.Storable as V
import Test.Instances.Finite ()

class Listable a where
  listOut :: [a]

instance Listable Bool where
  listOut = [False, True]

data Action n = Toggle (Finite n) | Swap (Finite n) (Finite n)
  deriving stock (Show, Read, Eq, Ord, Generic)

instance (KnownNat n) => Listable (Finite n) where
  listOut = finites

instance (KnownNat n) => Listable (Action n) where
  listOut = (Toggle <$> listOut) <> (Swap <$> listOut <*> listOut)

instance (KnownNat n, Monad m) => Serial m (Action n)

instance (Listable a, Listable b) => Listable (a, b) where
  listOut = (,) <$> listOut <*> listOut

newtype Pair a = Pair {toTuple :: (a, a)}
  deriving stock (Functor)
  deriving newtype (Ord, Eq, Show, Read, Generic, Listable)

instance Serial m a => Serial m (Pair a)

data Instruction n = Instruction {control :: Pair (Bool, Finite n), action :: Action n}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Listable (Instruction 8) where
  listOut = L.filter isValid (Instruction <$> listOut <*> listOut)

instance (KnownNat n, Monad m) => Serial m (Instruction n)

isValid :: Instruction 8 -> Bool
isValid Instruction {..} = 
  pairwiseDistinct indexes && (a < 6) && (b < 6)
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

type MyConstraint (n :: Nat) (m :: Type -> Type) a = (KnownNat n, MonadReader (MutVar (PrimState m) a) m , PrimMonad m, BitsN n a)

runInstruction :: MyConstraint n m a => Instruction n -> m ()
runInstruction Instruction {..} =
  do
    let Pair (a,b) = control
    a' <- (== fst a) <$> readV (snd a)
    b' <- (== fst b) <$> readV (snd b)
    when (a' && b') $
      case action of
        Toggle i -> toggleM i
        Swap i j -> swapM i j

readV :: MyConstraint n m a => Finite n -> m Bool
readV (i :: (Finite n)) = do
  v <- ask
  (`testBit'` i) <$> readMutVar v 

writeV :: MyConstraint n m a => Finite n -> Bool -> m ()
writeV i b = do
  v <- ask
  let f = if b then setBit' else clearBit'
  modifyMutVar' v (`f` i)

toggleM :: MyConstraint n m a => Finite n -> m ()
toggleM i = do
  v <- ask
  modifyMutVar' v (`complementBit'` i)

swapM :: MyConstraint n m a => Finite n -> Finite n -> m ()
swapM i j = do
  ixValue <- readV i
  jxValue <- readV j
  writeV i jxValue
  writeV j ixValue

funToByteStream :: (Word8 -> Word8) -> ByteString
funToByteStream f = toStrict . toLazyByteString $ V.foldl' (\ sofar a -> sofar <> word8 a) mempty v
  where
  maxWord8 :: Int
  maxWord8 = fromIntegral (maxBound :: Word8)
  toWord8 :: Int -> Word8
  toWord8 = fromIntegral
  v = V.generate maxWord8 (\ x -> toWord8 x `xor` f (toWord8 x))

allPrograms :: Int -> [[Instruction 8]]
allPrograms len = Relude.replicateM len listOut

runProgram :: [Instruction 8] -> Word8 -> Word8
runProgram p start = runST $ do
  r <- newMutVar start
  runReaderT (runInstruction `Relude.mapM_` p) r
  readMutVar r

