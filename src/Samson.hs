{-# Language DeriveFunctor, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# Language NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Samson (WireType, wireTypeBoom) where
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH
import Control.Category ((.), id)
import Relude hiding ((.), id)
import Data.Bits (Bits(xor))
import Control.Applicative (Applicative)

data AddMul = Add | Mul
  deriving (Eq, Show, Read, Ord)

$(makeBoomerangs ''AddMul)

data WireType a = Pure a | Ident AddMul | Inv AddMul (WireType a) | BiOp AddMul (WireType a) (WireType a)
  deriving (Eq, Functor)

$(makeBoomerangs ''WireType)

pattern Zero = Ident Add
pattern One = Ident Mul
pattern Neg a = Inv Add a
pattern Under a = Inv Mul a
pattern AddOp a b = BiOp Add a b
pattern MulOp a b = BiOp Mul a b

wireTypeBoom :: (forall a . StringBoomerang a (c :- a)) -> StringBoomerang b (WireType c :- b)
wireTypeBoom b 
  = rPure . b
  <> rIdent . rAdd . "0"
  <> rIdent . rMul . "1"
  <> rInv . "-(" . rAdd . wireTypeBoom b . ")" 
  <> rInv . "(" . rMul . wireTypeBoom b . ")⁻¹" 
  <> rInv . "1/(" . rMul . wireTypeBoom b . ")" 
  <> rBiOp . "(" . rAdd . wireTypeBoom b . "+" . wireTypeBoom b . ")"
  <> rBiOp . "(" . rMul . wireTypeBoom b . "*" . wireTypeBoom b . ")"

instance Applicative WireType where
  pure = Pure
  Pure f <*> b = f <$> b
  Inv op f <*> b = Inv op (f <*> b)
  BiOp op f g <*> b = BiOp op (f <*> b) (g <*> b)
  Ident op <*> _ = Ident op

join' :: WireType (WireType a) -> WireType a
join' (Ident op) = Ident op
join' (Pure x) = x
join' (Inv op x) = Inv op (join x)
join' (BiOp op x y) = BiOp op (join x) (join y)

instance Monad WireType where
  m >>= f = join' (f <$> m)

data AsymRule where
  Assoc :: AddMul -> AsymRule
  Unit :: AddMul -> AsymRule
  Cup :: AddMul -> AsymRule
  Absorb :: AsymRule
  Distrib :: AsymRule
  
data Samson where
  Id :: Samson
  Swap :: AddMul -> Samson
  L :: AsymRule -> Samson
  R :: AsymRule -> Samson

domain, codomain :: Samson -> WireType a -> WireType a -> WireType a -> WireType a 
domain x a b c = fst (toWireTypePair x a b c)
codomain x a b c = snd (toWireTypePair x a b c)

toWireTypePair :: Samson -> WireType a -> WireType a -> WireType a -> (WireType a, WireType a)
toWireTypePair Id a _ _ = (a, a)
toWireTypePair (Swap op) a b _ = (BiOp op a b, BiOp op b a)
toWireTypePair (L (Assoc op)) a b c = (BiOp op a (BiOp op b c), BiOp op (BiOp op a b) c)
toWireTypePair (L (Unit op)) a _ _ = (BiOp op (Ident op) a, a)
toWireTypePair (L (Cup op)) a _ _ = (BiOp op a (Inv op a), Ident op)
toWireTypePair (L Absorb) a _ _ = (MulOp Zero a, Zero)
toWireTypePair (L Distrib) a b c = (MulOp a (AddOp b c), AddOp (MulOp a b) (MulOp a c))
toWireTypePair (R x) a b c = let ~(l, r) = toWireTypePair (L x) a b c in (r, l)