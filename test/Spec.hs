{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where
import Relude
import Data.List (nub)
import Cartagrapher
    ( isValid, pairwiseDistinct, swapM, Instruction, hammingWeight, toggleM)
import Data.Vector.Unboxed.Mutable.Sized as VMS ( copy, new )
import Data.Vector.Unboxed.Sized as VS
import Test.SmallCheck ( smallCheck, exists, forAll )
import Control.Monad.ST (runST)
import Data.Finite (Finite, finites)

main :: IO ()
main = do
  smallCheck 5 (exists (isValid :: Instruction 5 -> Bool)) 
  smallCheck 5 (exists (not . isValid :: Instruction 5 -> Bool))
  smallCheck 5 (forAll (\ (l :: [Word]) -> pairwiseDistinct l == (l == nub l)))
  smallCheck 5 (forAll (\ l -> withSizedList l (\ v -> fromIntegral (hammingWeight v) <= VS.length v )))
  smallCheck 5 (forAll checkSwap0)
  smallCheck 5 (forAll checkSwap1)
  smallCheck 5 (forAll checkSwap2)
  smallCheck 3 (forAll checkSwap3)
  smallCheck 100 (forAll checkToggle0)
  smallCheck 100 (forAll checkToggle1)
  
checkSwap0 :: VS.Vector 5 Bool -> Finite 5 -> Finite 5 -> Bool
checkSwap0 vStart ix iy =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        swapM ix iy
        swapM ix iy
      vEnd <- VS.freeze v
      pure (vStart == vEnd)

checkSwap1 :: VS.Vector 5 Bool -> Finite 5 -> Finite 5 -> Bool
checkSwap1 vStart ix iy =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        swapM ix iy
      vEnd <- VS.freeze v
      pure $ (vStart == vEnd) == (VS.index vStart ix == VS.index vStart iy)

checkSwap2 :: VS.Vector 5 Bool -> Finite 5 -> Finite 5 -> Bool
checkSwap2 vStart ix iy =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        swapM ix iy
      vEnd <- VS.freeze v
      pure $ (VS.index vStart ix == VS.index vEnd iy) && (VS.index vStart iy == VS.index vEnd ix)

checkSwap3 :: VS.Vector 5 Bool -> [(Finite 5, Finite 5)] -> Bool
checkSwap3 vStart l =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ Relude.mapM_ (uncurry swapM) l
      vEnd <- VS.freeze v
      pure $ hammingWeight vStart == hammingWeight vEnd

checkToggle0 :: VS.Vector 5 Bool -> Finite 5 -> Bool
checkToggle0 vStart ix =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        toggleM ix
        toggleM ix
      vEnd <- VS.freeze v
      pure (vStart == vEnd)

checkToggle1 :: VS.Vector 5 Bool -> Finite 5 -> Bool
checkToggle1 vStart ix =
    runST $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        toggleM ix
      vEnd <- VS.freeze v
      pure $ Relude.all 
        (\ j ->
          let start = VS.index vStart j in
          let end = VS.index vEnd j in 
            if j == ix then start /= end else start == end)
        finites