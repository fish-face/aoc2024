module Main where

import Data.Array
import Data.List
import Data.Ix

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

main :: IO ()
main = do
    input <- readInputLines
    let grid = fromLines (filter (not . null) input)
        dirIterators = [
            iterateEast,
            iterateWest,
            iterateNorth,
            iterateSouth,
            iterateNorthEast,
            iterateNorthWest,
            iterateSouthWest,
            iterateSouthEast ]
        (_, lr) = bounds grid
    print (sum $ map (countInDir grid) dirIterators)
    print (sum $ map fromEnum $ map (searchAt grid) (range ((0, 0), lr - (2, 2))))

countOccurrences :: Eq a => [a] -> [a] -> Int
countOccurrences pattern str = sum $ map fromEnum $ occurrences str where
    occurrences [] = [null pattern]
    occurrences str' = isPrefixOf pattern str':occurrences (tail str')

countInDir :: Array (Int, Int) Char -> DirIterator -> Int
countInDir grid dirIterator =
    let coords = (dirIterator $ bounds grid)
        slices = map (map (grid !)) coords --`debug` (unlines $ map show coords) :: [String]
        target = "XMAS" in
    sum $ map (countOccurrences target) slices --`debug` (unlines $ map show slices)

searchAt :: Array (Int, Int) Char -> Coord -> Bool
searchAt grid origin =
    let
        allCornerCoords = allRotationsAround (origin + (1,1)) (corners origin)
        cornersMatch = any (\rot -> map (grid !) rot == "SSMM") allCornerCoords
    in
    grid ! (origin + (1, 1)) == 'A' && cornersMatch

corners :: Coord -> [Coord]
corners origin = [origin, origin + (2, 0), origin + (0, 2), origin + (2, 2)]
