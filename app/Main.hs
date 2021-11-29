{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Main (main) where

import Relude
import Cartagrapher
import Data.Trie as T


main :: IO ()
main = do
  forM_ [1 ..] $ 
    \ n -> do
      putTextLn "Press Enter to continue:"
      _ <- getLine
      putTextLn $ "Starting row " <> show n
      l <- allPrograms n
      print (Relude.null l)
      let assocList = (\ p -> (funToByteStream (runProgram p) ,p)) <$> l
      s <- pure $! (T.size . T.fromList $ assocList)
      putTextLn $ 
        show s <> " unique functions generated by " <> show n <> " instructions"
