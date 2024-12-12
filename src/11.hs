{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.MemoUgly
import Data.Maybe
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

main :: IO ()
main = do
    input <- readInput
    let
        nums = map readInt $ C.words $ C.strip input
    print $ sum $ map (numStones 25) nums
    print $ sum $ map (numStones 75) nums

numStones :: Int -> Int -> Int
numStones = curry $ memo  $ uncurry numStones'
    where
        numStones' 0 _ = 1
        numStones' n stone =
            let
                d = ndigits stone
                (a, b) = splitdigits d stone
            in
            if
            | stone == 0 -> numStones (n-1) 1
            | d `mod` 2 == 0 -> numStones (n-1) a + numStones (n-1) b
            | otherwise -> numStones (n-1) (stone * 2024)

ndigits num = ceiling (logBase 10 (fromIntegral (num + 1)))
splitdigits digits num = divMod num (10 ^ (digits `div` 2))
