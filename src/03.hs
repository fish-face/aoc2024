module Main where

import Text.Regex.PCRE
import Data.List
import Data.Char

import Advent.Input

main :: IO ()
main = do
    input <- readInput
    let stripped = strip input
    print $ sumProducts $ part1 stripped
    print $ sumProducts $ part1 $ concat $ part2 stripped

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

part1regex = "mul\\(([0-9]+),([0-9]+)\\)"
part1 :: String -> [(Int, Int)]
part1 line = let parsed = line =~ part1regex :: [[String]]
                 nums = map (map (read) . tail) parsed :: [[Int]] in
    [(match !! 0, match !! 1) | match <- nums]

-- I hate pcre: magic (?s) to make . match \n
part2regex = "(?s)do\\(\\).*?don't\\(\\)"
part2 :: String -> [String]
-- wrap in do() and don't() to make regex simpler
part2 input = let input' = ("do()" ++ input ++ "don't()")
                  matches = (input' =~ part2regex) :: [[String]] in
    concat matches

sumProducts :: [(Int, Int)] -> Int
sumProducts pairs = sum $ map (uncurry (*)) pairs
