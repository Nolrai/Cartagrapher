{-# Language DerivingVia, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables, ConstraintKinds, DataKinds, KindSignatures, GADTs, TypeOperators, NoStarIsType #-}
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
import Test.SmallCheck.Series ( Serial(..) )
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Control.Monad.ST.Strict as ST
import Data.STRef.Strict as ST
import Data.Finite
import GHC.TypeLits
import Data.ByteString
import Data.Bits
import Data.BitsN
import Foreign.Ptr (castPtr)
import qualified GHC.IO.FD as ST

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

type MyConstraint (n :: Nat) m a = (KnownNat n, MonadReader (STRef m a) m , PrimMonad m, BitsN n a)

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
  (`testBit'` i) <$> ST.readSTRef v 

writeV :: MyConstraint n m => Finite n -> Bool -> m ()
writeV i b = do
  v <- ask
  let f = if b then setBit' else clearBit'
  ST.modifySTRef' v (`f` i)

toggleM :: MyConstraint n m => Finite n -> m ()
toggleM i = do
  v <- ask
  ST.modifySTRef v (`complementBit'` i)

swapM :: MyConstraint n m => Finite n -> Finite n -> m ()
swapM i j = do
  ixValue <- readV i
  jxValue <- readV j
  writeV i jxValue
  writeV j ixValue