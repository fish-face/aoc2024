{-# LANGUAGE MultiWayIf #-}

module Main where

--import Data.Array
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInputLines
    let (rules, updates) = parse input
    print $ part1 rules updates
    print $ part2 rules updates

parse :: [B.ByteString] -> ([(Int, Int)], [[Int]])
parse input = let (rules, updates) = break B.null $ input in
    (map parseRule rules, map parseUpdate $ filter (not . B.null) updates)

parseRule :: B.ByteString -> (Int, Int)
parseRule line = let nums = map (fst . unwrap . C.readInt) $ B.split 124 line :: [Int] in -- '|'
    (head nums, head $ tail nums)

parseUpdate :: B.ByteString -> [Int]
parseUpdate line = map (fst . unwrap . C.readInt) $ B.split 44 line -- ','

part1 :: [(Int, Int)] -> [[Int]] -> Int
part1 rules updates = sum $ [middle update| update <- updates, checkUpdate rules update]

middle :: [Int] -> Int
middle update = update !! (length update `div` 2)

checkUpdate :: [(Int, Int)] -> [Int] -> Bool
checkUpdate _ [] = True
checkUpdate rules (a:bs) = and [checkOrder rules a b | b <- bs] && checkUpdate rules bs

checkOrder :: [(Int, Int)] -> Int -> Int -> Bool
checkOrder rules a b = not $ (b, a) `elem` rules

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 rules updates = sum $ [middle $ sortUpdates rules update| update <- updates, not $ checkUpdate rules update]

sortUpdates :: [(Int, Int)] -> [Int] -> [Int]
sortUpdates rules update = sortBy (\a -> \b -> if
        | (a, b) `elem` rules -> LT
        | otherwise -> GT) update
