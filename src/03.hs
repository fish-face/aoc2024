{-# LANGUAGE OverloadedStrings, QuasiQuotes  #-}

module Main where

import Text.Regex.PCRE.Heavy
import Data.List
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Debug.Trace
import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInput
    let stripped = C.strip input
    print $ sumProducts $ part1 stripped
    print $ sumProducts $ part1 $ B.concat $ part2 stripped

--strip :: String -> String
--strip = dropWhileEnd isSpace . dropWhile isSpace
--
part1regex = [re|mul\(([0-9]+),([0-9]+)\)|]
part1 :: B.ByteString -> [(Int, Int)]
--part1 line = let parsed = line =~ part1regex :: [[B.ByteString]]
part1 line = let parsed = part1regex `scan` line :: [(B.ByteString, [B.ByteString])]
                 nums = map (map (fst . unwrap . C.readInt) . snd) parsed :: [[Int]] in
    [(match !! 0, match !! 1) | match <- nums]

-- I hate pcre: magic (?s) to make . match \n
part2regex = [re|(?s)do\(\).*?don't\(\)|]
part2 :: B.ByteString -> [B.ByteString]
-- wrap in do() and don't() to make regex simpler
part2 input = let input' = B.concat ["do()", input, "don't()"] :: B.ByteString
                  matches = (part2regex `scan` input') :: [(B.ByteString, [B.ByteString])] in
    map fst matches

sumProducts :: [(Int, Int)] -> Int
sumProducts pairs = sum $ map (uncurry (*)) pairs
