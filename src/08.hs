module Main where

import Debug.Trace

import Data.Set hiding (map, filter)
import qualified Data.Set as Set
import Data.Map hiding (map, filter, unions)
import qualified Data.Map as Map

import Data.Ix
import Data.List
import Data.Tuple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

main :: IO ()
main = do
    input <- readInputLines
    let
        bounds = ((0, 0), (C.length (head input) - 1, (length input) - 1))
        coordinated = withCoords input
        whattocallthis = map (\(a, b) -> (b, [a])) $ filter (\(loc, c) -> c /= '.') coordinated
        locations = fromListWith (++) whattocallthis
    print $ Set.size $ part1 bounds locations
    print $ Set.size $ part2 bounds locations

printPoints :: Set Coord -> String
printPoints points =
    let
        pointlist = Set.elems points :: [Coord]
        width = maximum $ map fst $ pointlist :: Int
        height = maximum $ map snd $ pointlist :: Int
        coords = range ((0, 0), (height, width))
    in
    concat [if x == width then [charAt (x, y), '\n'] else [charAt (x, y)] | (y, x) <- coords]
    where
          charAt :: Coord -> Char
          charAt pos = if Set.member pos points then 'X' else '.'

part1 :: (Coord, Coord) -> Map Char [Coord] -> Set Coord
part1 bounds locations = unions $ Map.map (findAntinodes bounds) locations

findAntinodes :: (Coord, Coord) -> [Coord] -> Set Coord
findAntinodes bounds locations = let
        pairs = [(a, b) | (a:rest) <- tails locations, b <- rest] :: [(Coord, Coord)]
        diffs = map (uncurry (-)) pairs :: [Coord]
        pairs_diffs = zip pairs diffs :: [((Coord, Coord), Coord)]
        potential = concat $ map doIt pairs_diffs
        inbounds = filter (inRange bounds) potential
    in
    Set.fromList inbounds
    where
        doIt :: ((Coord, Coord), Coord) -> [Coord]
        doIt ((a, b), d) = [a + d, b - d]

part2 :: (Coord, Coord) -> Map Char [Coord] -> Set Coord
part2 bounds locations = unions $ Map.map (findAntinodes2 bounds) locations

findAntinodes2 :: (Coord, Coord) -> [Coord] -> Set Coord
findAntinodes2 bounds locations = let
        pairs = [(a, b) | (a:rest) <- tails locations, b <- rest] :: [(Coord, Coord)]
        diffs = map (uncurry (-)) pairs :: [Coord]
        pairs_diffs = zip pairs diffs :: [((Coord, Coord), Coord)]
        result = concat $ map doIt pairs_diffs
    in
    Set.fromList result
    where
        doIt :: ((Coord, Coord), Coord) -> [Coord]
        doIt ((a, b), d) = let
                forward = accumUntil (not . inRange bounds) (+d) a
                backward = accumUntil (not . inRange bounds) (subtract d) a
            in forward ++ backward

accumUntil :: (t -> Bool) -> (t -> t) -> t -> [t]
accumUntil p f a = a:go a
  where
    go x | p (f x)      = []
         | otherwise    = accumUntil p f (f x)
