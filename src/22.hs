{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInputLines
    let
        inputNums = map (fst . fromJust . C.readInt) input
        secretSeqs = map evolveSecret inputNums :: [[Int]]
        priceSeqs = map (\seq -> map price seq) secretSeqs :: [[Int]]
        changeSeqs = map (\seq -> (sequence4s (changes (map price seq)))) priceSeqs :: [[(Int, Int, Int, Int)]]
        seqToPrice = map (\(prices, changes) -> M.fromListWith (\_ b -> b) $ zip (take 2000 changes) (drop 4 prices)) $ zip priceSeqs changeSeqs :: [Map (Int, Int, Int, Int) Int]
        totals = foldl' (\ma mb -> M.unionWith (+) ma mb) M.empty seqToPrice :: Map (Int, Int, Int, Int) Int
    print $ sum $ map (\seq -> seq !! 2000) secretSeqs
    print $ maximum $ M.elems totals

evolveSecret :: Int -> [Int]
evolveSecret x = iterate Main.step x

step :: Int -> Int
step x = let
        x1 = prune $ mix x $ x `shiftL` 6
        x2 = prune $ mix x1 $ x1 `shiftR` 5
        x3 = prune $ mix x2 $ x2 `shiftL` 11
    in x3

mix x y = x `xor` y

prune x = x .&. 16777215

price x = x `mod` 10

changes :: [Int] -> [Int]
changes xs = map (uncurry $ flip (-)) $ zip xs $ tail xs

sequence4s :: [Int] -> [(Int, Int, Int, Int)]
sequence4s seq = map flatten4 $ zip (tails seq !! 3) (zip (tails seq !! 2) (zip (tails seq !! 1) seq))

flatten4 :: (a, (b, (c, d))) -> (d, c, b, a)
flatten4 (a, (b, (c, d))) = (d, c, b, a)
