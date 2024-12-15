{-# LANGUAGE OverloadedStrings, QuasiQuotes  #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Text.Regex.PCRE.Heavy
import qualified Data.ByteString.Char8 as C
import Data.List.Split

import Advent
import Advent.Input
import Advent.Coord

data Machine = Machine Int Int Int Int Int Int
    deriving (Show, Eq)

main :: IO ()
main = do
    input <- readInputLines
    let machines = map parse $ chunksOf 4 input
    print $ sum $ map (solve 0) machines
    print $ sum $ map (solve 10000000000000) machines

part1regex = [re|X.([0-9]+), Y.([0-9]+)|]
intGroups :: [(C.ByteString, [C.ByteString])] -> Coord
intGroups match = let
        results = head $ map (map (fst . unwrap . C.readInt) . snd) match
    in
    (results !! 0, results !! 1)

parse :: [C.ByteString] -> Machine
parse machineInput = let
        a = C.drop 10 $ machineInput !! 0
        b = C.drop 10 $ machineInput !! 1
        prize = C.drop 7 $ machineInput !! 2
        (ax, ay) = intGroups (part1regex `scan` a)
        (bx, by) = intGroups (part1regex `scan` b)
        (tx, ty) = intGroups (part1regex `scan` prize)
    in
    Machine ax ay bx by tx ty

solve :: Int -> Machine -> Int
solve offset (Machine ax ay bx by tx ty) = let
        impossible = (ax*by - ay*bx) == 0
        tx' = tx + offset
        ty' = ty + offset
        integer = (-bx*ty' + by*tx') `mod` (ax*by - ay*bx) == 0 && (ax*ty' - ay*tx') `mod` (ax*by - ay*bx) == 0
        a = (-bx*ty' + by*tx') `div` (ax*by - ay*bx)
        b = (ax*ty' - ay*tx') `div` (ax*by - ay*bx)
    in
    if impossible || not integer then 0
    else 3 * a +  b
