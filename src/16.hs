{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

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
        path = pathFind grid start end
    print start
    print end
    print path

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

--type State = (Int, Vector Coord, Coord)
data State = State {
    seen :: Set Position,
    distance :: Map Position Int,
    queue :: Q.MinQueue (Int, Position)
}

pathFind :: Grid Char -> Coord -> Coord -> Int
pathFind grid start end = let
        seen = S.empty
        distance = M.singleton (East, start) 0
        queue = Q.singleton (0, (East, start))
        result = traceShowId $ go (State seen distance queue)
    in fromJust $ lookupAnyDir result end
    where
        go :: State -> Map Position Int
        go state@(State seen distance queue) = if Q.null queue then distance `debug` "ran out"
            else let
                    ((d, pos@(_, p)), queue1) = Q.deleteFindMin queue
                in
                if
                    | p == end -> distance `debug` "success"
                    | S.member pos seen -> go (state {queue=queue1}) `debug` "already seen"
                    | otherwise -> let
                            seen1 = S.insert pos seen
                            nexts = getNeighbourCosts pos
                            unvisited = filter (\(d1, p) -> not $ S.member p seen1) nexts
                        in
                        go $ foldl (foldNeighbour pos) (State seen1 distance queue1) unvisited
        foldNeighbour :: Position -> State -> (Int, Position) -> State
        foldNeighbour pos state@(State seen1 distance queue1) (stepCost, neighbour) = let
                newDist = distance M.! pos + stepCost
                oldDist = distance M.! neighbour --`debug` (show newDist)
            in if not (M.member neighbour distance) || newDist < oldDist
                then State seen1 (M.insert neighbour newDist distance) (Q.insert (newDist, neighbour) queue1)
                else state
        getNeighbourCosts :: Position -> [(Int, Position)]
        getNeighbourCosts (d, p) = let
                dirs = [rotateDirR d, rotateDirL d]
                p1 = step p d
                coords = if grid ! p1 /= '#' then [(1, (d, p1))] else []
            in
            coords ++ [(1000, (rotateDirR d, p)), (1000, (rotateDirL d, p))]
        lookupAnyDir :: Map Position Int -> Coord -> Maybe Int
        lookupAnyDir m pos = firstJusts $ map (\d -> M.lookup (d, pos) m) [North, East, South, West]
