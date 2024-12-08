module Main where

import Debug.Trace
import Prelude hiding (lookup)
--import qualified Data.Set as Set (Set, insert, size, empty)
import Data.Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import Data.Array hiding (assocs)
import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord
import Control.Exception (ArithException(LossOfPrecision))

main :: IO ()
main = do
    input <- readInputLines
    let
        (pos, dir) = findStart input
        grid = fromLines input
    let Leave res histP histPD = walk grid Map.empty Map.empty (pos, dir)
    print res
    print $ part2 grid (pos, dir) histP histPD

findStart :: [C.ByteString] -> (Coord, Direction)
findStart input = let
        nested = findIV isJust [findIVBS isDirection line|line <- input]
        outer = unwrap nested :: (Int, Maybe (Int, Char))
        inner = unwrap $ snd outer :: (Int, Char)
        (x, c) = inner
    in
    ((x, fst outer), dirFromChar c)


findIV :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIV p = find (\(i, v) -> p v) . zip [0..]

findIVBS :: (Char -> Bool) -> C.ByteString -> Maybe (Int, Char)
findIVBS p s = let res = C.findIndex p s in
    case res of
        Just idx -> Just (idx, s `C.index` idx)
        Nothing -> Nothing

--walk :: Array Coord Char -> Coord -> Direction -> Maybe (Coord, Direction)
--walk grid pos dir = let
--        next = step pos dir in
--    if inRange (bounds grid) next then
--        if grid ! next == '#' then
--            Just (step pos (rotateDirR dir), rotateDirR dir)
--        else
--            Just (next, dir)
--    else Nothing

data WalkResult = Leave Int (Map Coord Int) (Map (Coord, Direction) Int) | Loop
    deriving (Show, Eq)

walk :: Grid Char -> Map Coord Int -> Map (Coord, Direction) Int -> (Coord, Direction) -> WalkResult
walk grid histP histPD (pos, dir) = go grid histP histPD (pos, dir) 0 where
    go :: Grid Char -> Map Coord Int -> Map (Coord, Direction) Int -> (Coord, Direction) -> Int -> WalkResult
    go grid histP histPD (pos, dir) n = let
            nexthistP = Map.Strict.insertWith (\a b -> b) pos n histP :: Map Coord Int
            nexthistPD = Map.insert (pos, dir) n histPD
--            nexthistPD = insert (pos, dir) histPD
            next = getNext grid pos dir in
        case next of
            Just pd ->
                if pd `Map.member` histPD then Loop --`debug` printPoints grid histP
                else go grid nexthistP nexthistPD pd (n+1) --`debug` (show pd ++ " " ++ (show (n + 1)))
            Nothing -> Leave (Map.size histP + 1) histP histPD

getNext :: Grid Char -> Coord -> Direction -> Maybe (Coord, Direction)
getNext grid pos dir = let
        candidates = Prelude.map (\d -> (step pos d, d)) [dir, rotateDirR dir, reflectDir dir]
        oob = find (\(p, d) -> not (inRange (bounds grid) p)) $ candidates
        found = find (\(p, d) -> ((grid ! p) /= '#')) $ candidates
    in
    if isJust oob then Nothing
    else found

printPoints :: Grid Char -> Map Coord a -> String
printPoints grid points =
    concat [if x == width then [charAt (x, y), '\n'] else [charAt (x, y)] | (x, y) <- coords]
    where (_, (_, width)) = bounds grid
          coords = concat $ iterateEast $ bounds grid
--          charAt pos = maybe (grid ! pos) (const 'X') (find (\p -> p == pos) points)
          charAt :: Coord -> Char
          charAt pos = if Map.member pos points then 'X' else (grid ! pos)

part2 :: Grid Char -> (Coord, Direction) -> Map Coord Int -> Map (Coord, Direction) Int -> Int
part2 grid start histP histPD = 1 + (sum $ Prelude.map (
        fromEnum . testLoop grid start histP
    ) $ Map.keys histP)
--part2 grid start histP histPD = go empty (Map.assocs histPD)
--    where go :: Set Coord -> [((Coord, Direction), Int)] -> Int
--          go found [] = 1 + size found
--          go found (x:xs) = go (testLoop grid found start histP x) xs

testLoop :: Grid Char -> (Coord, Direction) -> Map Coord Int -> Coord -> Bool
testLoop grid start histP obstacle =
    let
        res = walk (grid // [(obstacle, '#')]) Map.empty Map.empty start
    in
    res == Loop

--testLoop grid found start histP ((p, dir), n) =
--    let
--        (startP, _) = start
--        candidateObstacle = p
----        res = walk (grid // [(candidateObstacle, '#')]) Map.empty Map.empty (p, rotateDirR dir)
--        res = walk (grid // [(candidateObstacle, '#')]) Map.empty Map.empty start
----        res = walk grid Map.empty (Map.singleton (p, dir) 0) (p, rotateDirR dir)
--        doIt = if res == Loop then --`debug` ("test " ++ show (candidateObstacle, p, dir) ++ " = " ++ (if res == Loop then show res else "Leave")) then
--            insert candidateObstacle found
--        else found
--        in
----    if candidateObstacle == startP || grid ! candidateObstacle == '#' then found
----    else case Map.lookup candidateObstacle histP of
----        Just m ->
------            if m < n then False -- we travelled through the candidate obstacle already so it would have altered the path getting here and cannot test it
----            if False then found
----            else doIt
----        otherwise -> doIt
--    doIt

-- get all the points in line with the obstacles, pointing towards the obstacle
--getCrossings ::Grid char -> Map (pos, dir) ()
--getCrossings grid = map getCrossingForPoint (Data.Array.assocs grid) where
--    getCrossingForPoint (pos, c) = case c of
--        '#' ->
--        _ -> []

--accumUntil :: (t -> Bool) -> (t -> t) -> t -> [t]
--accumUntil p f a = a:go a
--  where
--    go x | p x          = []
--         | otherwise    = accumUntil p f (f x)
