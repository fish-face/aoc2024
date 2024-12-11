{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Function.Memoize
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
    print $ sum $ map (memoize numStones 75) nums

numStones :: Int -> Int -> Int
numStones = memoize2 numStones'
    where
        numStones' 0 stone = 1
        numStones' n stone =
            let
                digs = digits stone
                ndigs = C.length digs
                (as, bs) = C.splitAt (ndigs `div` 2) digs
                a = readInt as
                b = readInt bs
            in
            if
            | stone == 0 -> numStones (n-1) 1 --`debug` ((replicate n ' ') ++ "zero " ++ (show 1))
            | ndigs `mod` 2 == 0 -> numStones (n-1) a + numStones (n-1) b --`debug` ((replicate n ' ') ++ "split " ++ (show (a, b)))
            | otherwise -> numStones (n-1) (stone * 2024) --`debug` ((replicate n ' ') ++ "mul " ++ (show (stone * 2048)))

digits :: Int -> C.ByteString
digits num = C.pack $ show num
