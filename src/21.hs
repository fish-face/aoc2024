{-# LANGUAGE FlexibleInstances #-}

module Main where

import Debug.Trace

import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input
import Advent.Coord

main :: IO ()
main = do
    input <- readInputLines
    let
        mostCosts = dirCosts dirCoords (0, 0)
        p1CostMap = dirCosts keyCoords (0, 3) $ (iterate mostCosts baseCosts) !! 2
        p1Costs = map ((applyCosts p1CostMap) . C.unpack) input
        p2CostMap = dirCosts keyCoords (0, 3) $ (iterate mostCosts baseCosts) !! 25
        p2Costs = map ((applyCosts p2CostMap) . C.unpack) input
        inputVals = map (fst . fromJust . C.readInt) input
    print $ sum $ map (\(x, y) -> x * y) $ zip p1Costs inputVals
    print $ sum $ map (\(x, y) -> x * y) $ zip p2Costs inputVals

genPaths :: Coord -> Coord -> [[Direction]]
genPaths start end = let
        (dx, dy) = end - start
        adx = abs dx
        ady = abs dy
        horizontal = replicate adx (vecDir (dx `div` adx, 0))
        vertical = replicate ady (vecDir (0, dy `div` ady))
    in nub $ permutations $ horizontal ++ vertical

dirCoord :: Direction -> Coord
dirCoord d = case d of
    North -> (1, 0)
    East -> (2, 1)
    South -> (1, 1)
    West -> (0, 1)
    _ -> error "how did this happen"

keyCoord :: Char -> Coord
keyCoord c = case c of
    '0' -> (1, 3)
    'A' -> (2, 3)
    '1' -> (0, 2)
    '2' -> (1, 2)
    '3' -> (2, 2)
    '4' -> (0, 1)
    '5' -> (1, 1)
    '6' -> (2, 1)
    '7' -> (0, 0)
    '8' -> (1, 0)
    '9' -> (2, 0)
    _ -> error ("not on keypad: " ++ show c)

keyCoords :: [Coord]
keyCoords = [
    (1, 3),
    (2, 3),
    (0, 2),
    (1, 2),
    (2, 2),
    (0, 1),
    (1, 1),
    (2, 1),
    (0, 0),
    (1, 0),
    (2, 0)]

accept:: Coord
accept = (2, 0)
dirCoords :: [Coord]
dirCoords = [(1,0), (2,1), (1,1), (0,1), (2, 0)]
baseCosts :: Map (Coord, Coord) Int
baseCosts = M.fromList $ [((p, q), 1) | p <- dirCoords, q <- dirCoords]

-- map[d,e] = the minimum cost of typing the directions on the keypad above, starting from d ending up at e
type CostMap = Map (Coord, Coord) Int

dirCosts :: [Coord] -> Coord -> CostMap -> CostMap
dirCosts layerCoords hole upperCosts = let
    --    allDirs = [North, South, East, West]
        -- find all pairs of starts and ends
        moves = [(p, q) | p <- layerCoords, q <- layerCoords]
        -- find the directions to input on the upper level to make the given move, and also filter out the ones which would go over the gap
        dirPaths = map (\(start, end) -> filterGaps start hole $ uncurry genPaths (start, end)) moves :: [[[Direction]]]
    --    dirPathsRev = map (\p -> genPaths p accept) dirCoords
        -- turn the directions into the coordinates of the buttons on the upper level
        coordPaths = map (map (map dirCoord)) dirPaths :: [[[Coord]]]
        -- prepend & append the "accept" button on the upper level
        coordPathsAccepted = map (map (\path -> (accept:path) ++ [accept])) coordPaths --`debug` (show $ (moves !! 5, dirPaths !! 5, coordPaths !! 5)) :: [[[Coord]]]
        -- find moves on upper level by pairing each coordinate with the next one
        upperMoves = map (map (\path -> zip path $ tail path)) coordPathsAccepted :: [[[(Coord, Coord)]]]
        -- find costs from previous mapping
        costs = map (map (map (\move -> upperCosts M.! move))) upperMoves :: [[[Int]]]
        -- find minimum costs
        minima = map (minimum . map sum) costs :: [Int]
    in M.fromList $ zip moves minima

applyCosts :: CostMap -> [Char] -> Int
applyCosts costs s = let
        -- prepend A button (start position)
        coords = (2, 3):map keyCoord s
        moves = zip coords $ tail coords :: [(Coord, Coord)]
        moveCosts = map (costs M.!) moves :: [Int]
    in sum moveCosts

filterGaps :: Coord -> Coord -> [[Direction]] -> [[Direction]]
filterGaps start banned paths = let
        pathCoords = walkAll start
        valid = notElem banned
    in map fst $ filter (\(_, coords) -> valid coords) $ zip paths $ map pathCoords paths
