{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import Data.Array (bounds, (!), range, inRange)
import Data.List (find)
import Data.Bits

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

import Twelve.Lut

-- coordinate -> index in region vector, region vector
data Regions = Regions (Map Coord Int) (Vector Region)
    deriving (Show)

hasPt :: Regions -> Coord -> Bool
hasPt (Regions rMap _) p = M.member p rMap
addPt :: Regions -> Grid Char -> Char -> Coord -> Regions
addPt (Regions rMap vec) grid c p = let
        nbs = neighboursIn (bounds grid) p
        matching = filter ((==c) . (grid !)) nbs
        existing = find (\x -> M.member x rMap) matching :: Maybe Coord
        (newVec, regIdx) = case existing of
            Just matchingCoord -> let idx = rMap M.! matchingCoord in
                (V.accum (addPtToRegion) vec [(idx, p)], idx)
            -- TODO both prepend AND append are O(n) because Vector is not smart enough to know when it can reuse the old data
            Nothing -> let region = newRegion c p in
                (V.snoc vec region, V.length vec)
    in
    Regions (M.insert p regIdx rMap) newVec

-- type of region, set of points
data Region = Region Char (Set Coord)
    deriving (Show)

--empty c = Region c S.empty
newRegion c p = Region c (S.singleton p)
addPtToRegion :: Region -> Coord -> Region
addPtToRegion (Region c ps) p = Region c (S.insert p ps)
size (Region _ ps) = S.size ps

main :: IO ()
main = do
    input <- readInputLines
    let
        grid = fromLines input
        (Regions _ rs) = regions grid
        scores = map (score grid) $ V.toList rs
--    print rs
    print $ sum $ map fst scores
    print $ sum $ map snd scores

regions :: Grid Char -> Regions
regions grid = go (Regions M.empty V.empty) (range $ bounds grid) where
    go :: Regions -> [Coord] -> Regions
    go regions [] = regions
    go regions (p:ps) = if hasPt regions p then go regions ps else
        let
            c = grid ! p
            newRegions = addPt regions grid c p
            next = matchingNeighbours grid p
        in
        go newRegions (next ++ ps)
--        unions $ map (go newRegions) (neighboursIn (bounds grid) p)

matchingNeighbours grid p = let
        c = grid ! p
        nbs = neighboursIn (bounds grid) p
    in filter ((==c) . (grid !)) nbs

score :: Grid Char -> Region -> (Int, Int)
score grid (Region c ps) = let
        perimcontrib p = 4 - (length $ matchingNeighbours grid p)
        perimeter = sum $ map perimcontrib $ S.elems ps
        area = S.size ps
--        isCorner p = not (grid ! (step p North) == c && grid ! (step p South) == c && grid ! (step p East /= c) &&
        corners = sum $ map (countCorners grid c) $ S.elems ps :: Int
    in
    (
        area * perimeter,
        area * corners
    )

countCorners :: Grid Char -> Char -> Coord -> Int
countCorners grid c p = lut ! ( neighbourBitPattern grid c p )

pointMatches :: Grid Char -> Char -> Coord -> Bool
pointMatches grid c p = inRange (bounds grid) p && grid ! p == c

neighbourBitPattern :: Grid Char -> Char -> Coord -> Int
neighbourBitPattern grid c p = sum $ map (\(i, b) -> fromEnum (pointMatches grid c b) `shiftL` i) $ zip [0..] (neighbours8 p)
