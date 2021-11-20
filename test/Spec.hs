{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where
import Relude
import Data.List (nub)
import Cartagrapher
    ( isValid, pairwiseDistinct, swapM, Instruction )
import Data.Vector.Unboxed.Mutable.Sized as VMS ( copy, new )
import Data.Vector.Unboxed.Sized as VS
    ( MVector, freeze, thaw, Vector )
import Test.SmallCheck ( smallCheck, exists, forAll )
import Control.Monad.ST (runST)
import Data.Finite (Finite)

main :: IO ()
main = do
  smallCheck 5 (exists (isValid :: Instruction 5 -> Bool)) 
  smallCheck 5 (exists (not . isValid :: Instruction 5 -> Bool))
  smallCheck 5 (forAll (\ (l :: [Word]) -> pairwiseDistinct l == (l == nub l)))
  smallCheck 5 (forAll checkSwap)
  
checkSwap :: VS.Vector 5 Bool -> Finite 5 -> Finite 5 -> Bool
checkSwap vStart ix iy =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        swapM ix iy
        swapM ix iy
      vEnd <- VS.freeze v
      pure (vStart == vEnd)