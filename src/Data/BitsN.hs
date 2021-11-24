{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitsN where

import Data.Bits
import GHC.TypeNats
import Data.Finite
import Data.Word

class (FiniteBits a, KnownNat n) => BitsN (n :: Nat) a | a -> n where

bit' :: BitsN n a => Finite n -> a
bit' n = bit (fromIntegral n)
setBit' :: BitsN n a => a -> Finite n -> a
setBit' a n = a `setBit` fromIntegral n
clearBit' :: BitsN n a => a -> Finite n -> a
clearBit' a n = a `setBit` fromIntegral n
complementBit' :: BitsN n a => a -> Finite n -> 
complementBit' a n = a `complementBit` fromIntegral n

instance BitsN 4 Word4 where
instance BitsN 8 Word8 where
instance BitsN 16 Word16 where
instance BitsN 32 Word32 where
instance BitsN 64 Word64 where