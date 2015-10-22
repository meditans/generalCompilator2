{-# OPTIONS_GHC -fdefer-typed-holes #-}


-- Imports

module Lib where

import Control.Monad.Omega
import Data.Foldable (asum, toList)
import Data.List (group, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Monad.State
import Control.Monad.Writer



-- Definitions and functions for BinTree

data BinTree = Leaf | Branch BinTree BinTree
             deriving (Show, Eq)

generationOrder :: BinTree -> BinTree -> Ordering
generationOrder Leaf                Leaf                = EQ
generationOrder Leaf                (Branch _ _)        = LT
generationOrder (Branch _ _)        Leaf                = GT
generationOrder b1@(Branch b11 b12) b2@(Branch b21 b22) = compare (depth b1) (depth b2)
                                                       <> generationOrder b11 b21
                                                       <> generationOrder b12 b22

depth :: BinTree -> Int
depth Leaf = 0
depth (Branch b1 b2) = 1 + max (depth b1) (depth b2)

allBinTrees :: Omega BinTree
allBinTrees = asum [pure Leaf, Branch <$> allBinTrees <*> allBinTrees]

sufficientBinTrees :: Int -> [BinTree]
sufficientBinTrees n = take n . uniq . take (4*n) $ runOmega allBinTrees

uniq :: [BinTree] -> [BinTree]
uniq = map head . group . sortBy generationOrder


-- Le mappe di traduzione T ed S

data TwoMaps = TwoMaps { t :: Map BinTree Int
                       , s :: Map Int Int
                       } deriving (Show)

type M a = WriterT [String] (StateT TwoMaps []) a

computation :: M a
computation = undefined

equation :: BinTree -> BinTree -> BinTree -> M a
equation b1 b2 b3 = do
  twoMaps <- get
  tell ["Checking for b1 in the state"]
  tB1 <- if M.member b1 (t twoMaps)
         then lift . lift . toList $ M.lookup b1 (t twoMaps)
         else _where
  return _what
