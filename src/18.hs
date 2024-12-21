{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List

import qualified Data.Ix as Ix
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.PQueue.Min as Q

import Numeric.Search.Range
import Text.Printf

import Advent
import Advent.Input
import Advent.Coord
import Advent.Grid

height = 71
width = 71
bounds = ((0, 0), (width-1, height-1))
steps = 1024
end = (width-1, height-1)

main :: IO ()
main = do
    input <- readInputLines
    let
        coords = map parseCoord input
        blankGrid = A.array bounds $ zip (Ix.range bounds) (repeat False)
        p1Grid = blankGrid A.// (zip (take steps coords) (repeat True))
        laterCoords = drop steps coords
        p2Grids = V.fromList $ reverse $ foldl addCoord [p1Grid] laterCoords

    print $ fromJust $ pathFind p1Grid (0, 0)

    let
        Just p2idx = searchFromTo (\i -> not $ pathFindable (p2Grids V.! i)) 0 ((length p2Grids) - 1)
        (p2x, p2y) = laterCoords !! (p2idx - 1)

    -- hack: I am off by one somewhere, oh well
    printf "%d,%d\n" p2x p2y

data State = State {
    seen :: Set Coord,
    distance :: Map Coord Int,
    queue :: Q.MinQueue (Int, Coord)
}

pathFind :: Grid Bool -> Coord -> Maybe Int
pathFind grid start = let
        seen = S.empty
        distance = M.singleton start 0
        queue = Q.singleton (0, start)
        result = dijkstra (State seen distance queue)
    in M.lookup end result
    where
        dijkstra :: State -> Map Coord Int
        dijkstra state@(State seen distance queue) = if Q.null queue then distance
            else let
                    ((d, p), queue1) = Q.deleteFindMin queue
                in
                if
                    | p == end -> distance
                    | S.member p seen -> dijkstra (state {queue=queue1})
                    | otherwise -> let
                            seen1 = S.insert p seen
                            nexts = filter (\p1 -> not $ grid A.! p1) $ neighboursIn bounds p
                            unvisited = filter (\p' -> not $ S.member p' seen1) nexts
                        in
                        dijkstra $ foldl (foldNeighbour p) (State seen1 distance queue1) unvisited
        foldNeighbour :: Coord -> State -> Coord -> State
        foldNeighbour p state@(State seen1 distance queue1) np = let
                cost1 = distance M.! p
                newCost = cost1 + 1
                oldCost = distance M.! np
            in if
                | not (M.member np distance) || newCost < oldCost ->
                    State seen1 (M.insert np newCost distance) (Q.insert (newCost, np) queue1)
                | otherwise -> state

pathFindable grid = isJust $ pathFind grid (0, 0)

addCoord :: [Grid Bool] -> Coord -> [Grid Bool]
addCoord (grid:grids) p = (grid A.// [(p, True)]:grid:grids)
