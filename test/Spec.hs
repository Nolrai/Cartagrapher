{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where
import Relude
import Cartagrapher
import Data.Vector.Unboxed.Mutable.Sized as VMS
import Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed.Sized as VS hiding (fromList)
import Data.Vector.Unboxed as V hiding (fromList)
import Test.SmallCheck
import Control.Monad.ST
import Data.Finite (Finite)

main :: IO ()
main = do
  -- smallCheck 5 (exists (isValid :: Instruction 5 -> Bool)) 
  -- smallCheck 5 (exists (not . isValid :: Instruction 5 -> Bool))
  -- smallCheck 5 (forAll (\ (l :: [Word]) -> pairwiseDistinct l == (l == nub l)))
  -- smallCheck 5 (forAll $ uncurry (==) . checkSwap)
  let v = (fromList [True, False, False, False, True] :: V.Vector Bool)
  let vs = (fromTuple (True, False, False, False, True) :: VS.Vector 5 Bool)
  v' <- testThawV v
  vs' <- testThawS vs
  print v'
  print vs'
  
testThawS :: VS.Vector 5 Bool -> Finite 5 -> Finite 5 -> (VS.Vector 5 Bool, VS.Vector 5 Bool) 
testThawS vStart ix iy =
    stToIO $ do
      v <- VMS.new
      (v' :: VS.MVector 5 s Bool) <- VS.thaw vStart
      VMS.copy v v'
      flip runReaderT v $ do
        swapM ix iy
        swapM ix iy
      vEnd <- VS.freeze v
      pure (vStart, vEnd)

testThawV :: V.Vector Bool -> Finite 5 -> Finite 5 -> (V.Vector Bool, V.Vector Bool) 
testThawV vStart ix iy =
    stToIO $ do
      v <- VM.new 5
      v' <- V.thaw vStart
      VM.copy v v'
      vEnd <- V.freeze v
      pure (vStart, vEnd)

testThaw :: V.Vector Bool -> IO (V.Vector Bool)
testThaw v = stToIO $ V.thaw v >> pure v

testThawSized :: VS.Vector 5 Bool -> IO (VS.Vector 5 Bool)
testThawSized v = stToIO $ VS.thaw v >> pure v