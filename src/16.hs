{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List
import Data.Maybe
import Data.Array ((!), (//), assocs, bounds)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.PQueue.Min as Q

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import GHC.Data.Maybe

import Advent
import Advent.Input
import Advent.Coord
import Advent.Grid

main :: IO ()
main = do
    input <- readInputLines
    let
        grid = fromLines input
        (_, (width, height)) = bounds grid
        start = findStart input
        end = (width - 1, 1)
        (part1, part2) = pathFind grid start end
    print part1
    print part2

findStart :: [C.ByteString] -> Coord
findStart gridLines = let
        nested = findIV isJust [C.findIndex (=='S') line|line <- gridLines]
        y = unwrap nested :: (Int, Maybe Int)
        x = unwrap $ snd y :: Int
    in
    (x, fst y)

findIV :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIV p = find (\(i, v) -> p v) . zip [0..]

type Position = (Direction, Coord)

data State = State {
    seen :: Set Position,
    distance :: Map Position (Int, [[Coord]]),
    queue :: Q.MinQueue (Int, Position)
}

pathFind :: Grid Char -> Coord -> Coord -> (Int, Int)
pathFind grid start end = let
        seen = S.empty
        distance = M.singleton (East, start) (0, [[start]])
        queue = Q.singleton (0, (East, start))
        result = dijkstra (State seen distance queue)
        result' = fromJust $ lookupAnyDir result end
        -- for part2
        allPoints = S.unions $ map (S.fromList) (snd result') :: Set Coord
    in (fst result', S.size allPoints)
    where
        dijkstra :: State -> Map Position (Int, [[Coord]])
        dijkstra state@(State seen distance queue) = if Q.null queue then distance
            else let
                    ((d, pos@(_, p)), queue1) = Q.deleteFindMin queue
                in
                if
                    | p == end -> distance
                    | S.member pos seen -> dijkstra (state {queue=queue1})
                    | otherwise -> let
                            seen1 = S.insert pos seen
                            nexts = getNeighbourCosts pos
                            unvisited = filter (\(d1, p) -> not $ S.member p seen1) nexts
                        in
                        dijkstra $ foldl (foldNeighbour pos) (State seen1 distance queue1) unvisited
        foldNeighbour :: Position -> State -> (Int, Position) -> State
        foldNeighbour pos@(d, p) state@(State seen1 distance queue1) (stepCost, neighbour@(nd, np)) = let
                (cost1, paths1) = distance M.! pos
                newCost = cost1 + stepCost
                -- prepend new point to all shortest paths to current point
                newPaths = map (np:) paths1
                (oldCost, oldPaths) = distance M.! neighbour
            in if
                | not (M.member neighbour distance) || newCost < oldCost ->
                    State seen1 (M.insert neighbour (newCost, newPaths) distance) (Q.insert (newCost, neighbour) queue1)
                | newCost == oldCost ->
                    -- don't need to enqueue neighbour if we just found an alternative path; we will search anyway
                    State seen1 (M.insert neighbour (newCost, newPaths ++ oldPaths) distance) queue1
                | otherwise -> state
        getNeighbourCosts :: Position -> [(Int, Position)]
        getNeighbourCosts (d, p) = let
                dirs = [rotateDirR d, rotateDirL d]
                p1 = step p d
                coords = if grid ! p1 /= '#' then [(1, (d, p1))] else []
            in
            coords ++ [(1000, (rotateDirR d, p)), (1000, (rotateDirL d, p))]
        lookupAnyDir :: Map Position a -> Coord -> Maybe a
        lookupAnyDir m pos = firstJusts $ map (\d -> M.lookup (d, pos) m) [North, East, South, West]
