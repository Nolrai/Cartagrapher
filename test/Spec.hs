{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where
import Relude
import Data.List (nub)
import Cartagrapher
    ( isValid, pairwiseDistinct, swapM, Instruction, toggleM)
import Test.SmallCheck ( smallCheck, exists, forAll )
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import Data.Finite (Finite)
import Data.Bits
import Data.BitsN

main :: IO ()
main = do
  smallCheck 5 (exists (isValid :: Instruction 5 -> Bool)) 
  smallCheck 5 (exists (not . isValid :: Instruction 5 -> Bool))
  smallCheck 5 (forAll (\ (l :: [Word]) -> pairwiseDistinct l == (l == nub l)))
  smallCheck 5 (forAll checkSwap0)
  smallCheck 5 (forAll checkSwap1)
  smallCheck 5 (forAll checkSwap2)
  smallCheck 3 (forAll checkSwap3)
  smallCheck 100 (forAll checkToggle0)
  smallCheck 100 (forAll checkToggle1)

checkAction action prop vStart =
  runST $ do
    r <- newSTRef vStart
    runReaderT action r
    vEnd <- readSTRef v
    pure (prop vStart vEnd)
  
checkSwap0 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap0 ix iy =
  checkAction (swapM ix iy >> swapM ix iy) (==)

checkSwap1 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap1 ix iy =
    checkAction (swapM ix iy)
      (\ vStart vEnd -> 
        (vStart == vEnd) == (VS.index vStart ix == VS.index vStart iy)
      )

checkSwap2 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap2 vStart ix iy =
    checkAction (swapM ix iy)
      (\ vStart vEnd -> 
        (vStart == vEnd) == (VS.index vStart ix == VS.index vStart iy)
      )
      (\ vStart vEnd -> 
        (VS.index vStart ix == VS.index vEnd iy) && (VS.index vStart iy == VS.index vEnd ix)
      )

checkSwap3 :: [(Finite 8, Finite 8)] -> Word8 -> Bool
checkSwap3 vStart l =
    checkAction 
      (Relude.mapM_ (uncurry swapM) l)
      (\ vStart vEnd -> 
        hammingWeight vStart == hammingWeight vEnd
      )

checkToggle0 :: Finite 8 -> Word8 -> Bool
checkToggle0 ix =
checkToggle0 ix =
  checkAction (toggle ix >> toggle ix) (==)

checkToggle1 :: Finite 8 -> Word8 -> Bool
checkToggle1 ix =
      let test vStart vEmd = Relude.all 
        (\ j ->
          let start = VS.index vStart j in
          let end = VS.index vEnd j in 
            if j == ix then start /= end else start == end)
        finites
      in checkAction (toggle ix) test