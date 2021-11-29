{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where
import Relude
import Data.List (nub)
import Cartagrapher
    ( isValid, pairwiseDistinct, swapM, Instruction, toggleM)
import Control.Monad.Primitive
import Data.Primative.MutVar
import Test.SmallCheck ( smallCheck, exists, forAll )
import Control.Monad.ST (runST, ST)
import Data.STRef
import Data.Finite (Finite, finites)
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

type STAction = forall s. ReaderT (STRef s Word8) (ST s) ()

checkAction :: STAction -> (Word8 -> Word8 -> Bool) -> Word8 -> Bool
checkAction action prop vStart =
  runST $ do
    r <- newMutVar vStart
    runReaderT action r
    vEnd <- readMutVar r
    pure (prop vStart vEnd)
  
checkSwap0 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap0 ix iy =
  checkAction (swapM ix iy >> swapM ix iy) (==)

checkSwap1 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap1 ix iy =
    checkAction (swapM ix iy)
      (\ vStart vEnd -> 
        (vStart == vEnd) == (testBit' ix vStart == testBit' iy vStart)
      )

checkSwap2 :: Finite 8 -> Finite 8 -> Word8 -> Bool
checkSwap2 ix iy =
    checkAction (swapM ix iy)
      (\ vStart vEnd -> 
        (testBit' ix vStart == testBit' iy vEnd) && (testBit' iy vStart == testBit' vEnd ix)
      )

checkSwap3 :: [(Finite 8, Finite 8)] -> Word8 -> Bool
checkSwap3 l =
    checkAction 
      (Relude.mapM_ (uncurry swapM) l)
      (\ vStart vEnd -> 
        popCount vStart == popCount vEnd
      )

checkToggle0 :: Finite 8 -> Word8 -> Bool
checkToggle0 ix =
  checkAction (toggleM ix >> toggleM ix) (==)

checkToggle1 :: Finite 8 -> Word8 -> Bool
checkToggle1 ix =
      let test vStart vEnd = 
            Relude.all 
              (\ j ->
                let start = testBit' vStart j in
                let end = testBit' vEnd j in 
                if j == ix then start /= end else start == end
              )
              finites
      in checkAction (toggleM ix) test