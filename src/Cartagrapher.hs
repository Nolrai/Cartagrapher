{- |
Copyright: (c) 2021 Chris Upshaw
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>

cartisian genetic programing
-}

module Cartagrapher
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

data GenomeFormat = 
  Genome {
  rows :: Int,
  columns :: Int,
  reachBack :: Int,
  functionTableSize :: Int,
  numFunctionInputs :: Int
}

data NodeSpec =
  NodeSpec {
    function :: Int,
    args :: [Int]
  }