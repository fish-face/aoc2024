{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
--{-# LANGUAGE GHC2021 #-}
--{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List
import Data.Bits

--import Data.Array (Array)
--import qualified Data.Array as A
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Array.Unboxed (Array, UArray)
--import Data.Array.MArray (modifyArray)
import qualified Data.Array.Unboxed as A

--import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as M
import Data.HashMap.Lazy (HashMap)
import qualified Data.IntMap as M
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInputLines
    let
        inputNums = map (fst . fromJust . C.readInt) input
        secretSeqs = map (evolveSecret 2001) inputNums
        priceSeqs = map (\seq -> map price $ A.elems seq) secretSeqs
        changeSeqs = map (\seq -> (sequence4s (changes seq))) priceSeqs -- :: [[(Int, Int, Int, Int)]]
        totals = makeTotals2 (map (take 1997) changeSeqs) (map (drop 4) priceSeqs) -- :: HashMap (Int, Int, Int, Int) Int
    print $ sum $ map (\seq -> seq A.! 2000) $ secretSeqs
    print $ maximum $ A.elems totals

--evolveSecret :: Int -> [Int]
--evolveSecret = iterate Main.step
evolveSecret :: Int -> Int -> UArray Int Int
evolveSecret num start = runSTUArray $ do
    result <- newArray (0, num-1) 0
    x <- newSTRef start
    forM_ [0..num-1] $ \i -> do
        xv <- readSTRef x
        writeArray result i xv
        writeSTRef x $ step xv
    return result

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

bounds = ((-9,-9,-9,-9),(9,9,9,9))
type Change = (Int, Int, Int, Int)

sequence4s :: [Int] -> [Change]
--sequence4s seq = map (\(a, b, c, d) -> (a+10)*8000 + (b+10)*400 + (c+10)*20 + (d+10)) $ zip4 (tails seq !! 3) (tails seq !! 2) (tails seq !! 1) seq
sequence4s seq = zip4 (tails seq !! 3) (tails seq !! 2) (tails seq !! 1) seq

makeSeqToPrice = map (\(prices, changes) -> M.fromListWith (\a b -> b) $ zip (take 1997 changes) (drop 4 prices))

makeTotals2 :: [[Change]] -> [[Int]] -> UArray Change Int
makeTotals2 changeSeqs priceSeqs = runSTUArray $ do
    totals :: STUArray s Change Int
        <- newArray bounds 0
    forM_ (zip changeSeqs priceSeqs) $ \(changeSeq, priceSeq) -> do
        seen :: STUArray s Change Bool
            <- newArray bounds False
        forM_ (zip changeSeq priceSeq) $ \(change, price) -> do
            alreadySeen <- readArray seen change
            if alreadySeen then return ()
            else do
                writeArray seen change True
                oldVal <- readArray totals change
                writeArray totals change (oldVal + price)
    return totals
