{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}

module Main (main) where

import Relude as P
import Cartagrapher
    ( runProgram, funToByteStream, allPrograms, Instruction )
import Data.Trie as T
import Data.Primitive
import Control.Monad.Primitive


main :: IO ()
main = do 
  v <- newMutVar mempty
  forever (loop v)

loop :: MutVar RealWorld (Trie [Instruction  8]) -> IO ()
loop v = do
  putTextLn "Press Enter to continue:"
  s <- getLine
  case s of
    "q" -> exitSuccess
    _ -> do
      putTextLn "Starting round."
      oldTrie <- readMutVar v
      l <- pure $! 
        if T.null oldTrie
          then allPrograms 1
          else pairUp (filter (not . P.null) (T.elems oldTrie))
      let numPrograms = P.length l
      print (P.take 2 l)
      putTextLn $ 
        show numPrograms <> " canidate programs of length " <> show (maybe 0 length (viaNonEmpty head (drop 1 l)))
      let assocList = (\ p -> (funToByteStream (runProgram p), p)) <$> (mempty : l)
      let newTrie = P.foldl' (\tree (key, item) -> insertIfNew key item tree) oldTrie assocList
      writeMutVar v newTrie
      let numFunctions = T.size newTrie
      putTextLn $ show numFunctions <> " unique functions from those programs."

pairUp :: [[a]] -> [[a]]
pairUp l = (<>) <$> l <*> l

insertIfNew = alterBy f
  where
    f _ _ (Just x) = Just x
    f _ a Nothing = Just a