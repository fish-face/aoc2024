{-# LANGUAGE MultiWayIf #-}

module Main where

--import Data.Array
import Data.Set (Set, member)
import qualified Data.Set as Set

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

type Rules = Set (Int, Int)

main :: IO ()
main = do
    input <- readInputLines
    let
        (rules, updates) = parse input
        (good, bad) = partition (checkUpdate rules) updates
    print $ part1 good
    print $ part2 rules bad

parse :: [B.ByteString] -> (Rules, [[Int]])
parse input = let (rules, updates) = break B.null input in
    (Set.fromList $ map parseRule rules, map parseUpdate $ filter (not . B.null) updates)

parseRule :: B.ByteString -> (Int, Int)
parseRule line = let nums = map (fst . unwrap . C.readInt) $ B.split 124 line :: [Int] in -- '|'
    (head nums, head $ tail nums)

parseUpdate :: B.ByteString -> [Int]
parseUpdate line = map (fst . unwrap . C.readInt) $ B.split 44 line -- ','

part1 :: [[Int]] -> Int
part1 updates = sum $ [middle update| update <- updates]

middle :: [Int] -> Int
middle update = update !! (length update `div` 2)

checkUpdate :: Rules -> [Int] -> Bool
checkUpdate _ [] = True
checkUpdate _ [a] = True
checkUpdate rules (a:bs) = not ((head bs, a) `member` rules) && checkUpdate rules bs

part2 :: Rules -> [[Int]] -> Int
part2 rules updates = sum $ [middle $ sortUpdates rules update| update <- updates]

sortUpdates :: Rules -> [Int] -> [Int]
sortUpdates rules = sortBy (\ a b -> (if (a, b) `member` rules then LT else GT))
