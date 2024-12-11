module Main where

import Debug.Trace

import Data.Array
import Data.Set hiding (map, filter)

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

main :: IO ()
main = do
    input <- readInputLines
    let
        grid = fromLines input
--        bounds = ((0, 0), (C.length (head input) - 1, (length input) - 1))

    print $ part1 $ fmap (readCharInt) grid
    print $ part2 $ fmap (readCharInt) grid

readCharInt :: Char -> Int
readCharInt c = fromEnum c - 48

part1 :: Grid Int -> Int
part1 grid = sum $ map (score grid) $ assocs grid

part2 :: Grid Int -> Int
part2 grid = sum $ map (score2 grid) $ assocs grid

score :: Grid Int -> (Coord, Int) -> Int
score grid (p, 0) = size (reachableNines grid 0 p)
score _ _ = 0

score2 :: Grid Int -> (Coord, Int) -> Int
score2 grid (p, 0) = reachableNines2 grid 0 p
score2 _ _ = 0

reachableNines2 :: Grid Int -> Int -> Coord -> Int
reachableNines2 _ 9 p = 1
reachableNines2 grid v p = let
        validNeighbours = filter (\q -> grid ! q == (v+1)) (neighboursIn (bounds grid) p)
    in
   sum $ map (reachableNines2 grid (v + 1)) validNeighbours

reachableNines :: Grid Int -> Int -> Coord -> Set Coord
reachableNines _ 9 p = singleton p
reachableNines grid v p = let
        validNeighbours = filter (\q -> grid ! q == (v+1)) (neighboursIn (bounds grid) p)
    in
    unions $ map (reachableNines grid (v + 1)) validNeighbours
