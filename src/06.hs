module Main where

import Debug.Trace
import Prelude hiding (lookup)
import qualified Data.Set as Set (Set, insert, size, empty)
import Data.Map hiding ((!), filter, map)
import Data.Array hiding (assocs)
import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe

import Advent
import Advent.Input
import Advent.Grid
import Advent.Coord

data History = History (Map (Coord, Direction) Int) (Map (Coord, Direction) Int)
    deriving (Show)

newHistory = History empty empty

seen (History s _) = s
collisions (History _ c) = c

insertStep (History seen collisions) pos dir n = History (insert (pos, dir) n seen) collisions
getStep (History seen _) (pos, dir) = lookup (pos, dir) seen
hasStep (History seen _) (pos, dir) = member (pos, dir) seen
insertCollision (History seen collisions) pos dir n = History seen (insert (pos, dir) n collisions)
getCollision (History _ collisions) pos dir = lookup (pos, dir) collisions
hasCollision (History _ collisions) (pos, dir) = member (pos, dir) collisions
colls (History _ collisions) = assocs collisions

main :: IO ()
main = do
    input <- readInputLines
    let
        (pos, dir) = findStart input
--        cleaned = clean input pos
        grid = fromLines input
    putStr $ toString grid
    print (pos, dir)
    print $ part1 grid Set.empty $ Just (pos, dir)
    print $ part2 grid newHistory $ Just (pos, dir)

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

walk :: Array Coord Char -> Coord -> Direction -> Maybe (Coord, Direction)
walk grid pos dir = let
        next = step pos dir in
    if inRange (bounds grid) next then
        if grid ! next == '#' then
            Just (step pos (rotateDirR dir), rotateDirR dir)
        else
            Just (next, dir)
    else Nothing

--part1 grid pos dir = let
--    next = walk grid pos dir in
--    maybe 0 (\next' -> 1 + part1 grid) next

part1 :: Array Coord Char -> Set.Set Coord -> Maybe (Coord, Direction) -> Int
part1 grid seen state = case state of
    Just (pos, dir) -> part1 grid (Set.insert pos seen) (walk grid pos dir)
    Nothing -> Set.size seen
--part1 _ Nothing = 0
--part1 grid Just (pos, dir) = 1 + part1 grid $ walk grid pos dir

--add pos set = let
--    origSize = size set
--    added = insert pos set in
--    if origSize == size added then
--        added `debug` (show pos)
--    else
--        added

walk2 :: Array Coord Char -> History -> Coord -> Direction -> Int -> (History, Maybe (Coord, Direction))
walk2 grid history pos dir n = let
        next = step pos dir in --`debug` show (pos, dir) in
    if inRange (bounds grid) next then
        if grid ! next == '#' then
--            let stepped = insertStep history (pos, dir) in
            (insertCollision history pos dir n, Just (pos, rotateDirR dir))
        else
            (insertStep history next dir n, Just (next, dir))
    else (history, Nothing)

part2 :: Array Coord Char -> History -> Maybe (Coord, Direction) -> Int
part2 grid history state =
    go grid history state 0 where
        go grid history state n = case state of
            Just (pos, dir) -> let (nexthist, nextstate) = walk2 grid history pos dir n in
                go grid nexthist nextstate (n + 1)
            Nothing -> traceShowId (part2analyse grid history)

part2analyse :: Array Coord Char -> History -> Int
part2analyse grid history = let
        canCreateLoop = map (analyseCollision grid history) (colls history) in
    sum canCreateLoop `debug` show (canCreateLoop)

analyseCollision :: Array Coord Char -> History -> ((Coord, Direction), Int) -> Int
analyseCollision grid history ((pos, dir), n) = let
        onRoute = accumUntil
            (\ p -> not (inRange (bounds grid) p) || grid ! p == '#')
            (`step` reflectDir dir)
            pos
        overlaps = map (isOverlap history n dir) onRoute `debug` ("collision at " ++ show (pos, dir) ++ "\n\t" ++ show (rotateDirL dir, onRoute)) in
    sum $ map fromEnum overlaps `debug` ("overlap\n\t" ++ show (map fromEnum overlaps))

isOverlap :: History -> Int -> Direction -> Coord -> Bool
isOverlap history n dir p = let candidate = (p, rotateDirL dir) in
        hasStep history candidate && unwrap (getStep history candidate) > n && not (hasCollision history candidate)
--    onRoute `debug` ("collision at " ++ show (pos, dir) ++ "\n\t" ++ show (rotateDirL dir, onRoute))

accumUntil :: (t -> Bool) -> (t -> t) -> t -> [t]
accumUntil p f a = a:go a
  where
    go x | p x          = []
         | otherwise    = accumUntil p f (f x)
