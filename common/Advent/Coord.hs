
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Advent.Coord where

import qualified Data.Set as Set
import Data.List
import Data.Ix
import qualified Data.ByteString.Char8 as C

import Advent

type Coord = (Int, Int)

instance Num Coord where
    (+) = zipCoord (+)
    (*) = zipCoord (*)
    (-) = zipCoord (-)
    abs = mapCoord abs
    signum = mapCoord signum
    fromInteger :: Integer -> Coord
    fromInteger i = (fromInteger i, fromInteger i)

--instance Ix Coord where
--    range ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1, y2]]
--
--    index :: (Coord, Coord) -> Coord -> Int
--    index ((x1, y1), (x2, y2)) (x,  y) = index (x1,x2) x * rangeSize (y1,y2) + index (y1,y2) y
----    unsafeIndex :: (Coord, Coord) -> Coord -> Int
----    unsafeIndex ((x1, y1), (x2, y2)) (x,  y) = unsafeIndex (x1,x2) x * unsafeRangeSize (y1,y2) + unsafeIndex (y1,y2) y
--    inRange :: (Coord, Coord) -> Coord -> Bool
--    inRange = _

mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (x, y) = (f x, f y)

zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

mapXY :: (Int -> Int) -> (Int -> Int) -> Coord -> Coord
mapXY f g (x, y) = (f x, g y)

flipCoord :: Coord -> Coord
flipCoord (x, y) = (y, x)

rotateR :: Coord -> Coord
rotateR (x, y) = (-y, x)

rotateL :: Coord -> Coord
rotateL (x, y) = (y, -x)

rotateLAround :: Coord -> Coord -> Coord
rotateLAround o p = rotateL (p - o) + o

rotateRAround :: Coord -> Coord -> Coord
rotateRAround o p = rotateR (p - o) + o

allRotations :: [Coord] -> [[Coord]]
allRotations points = take 4 $ iterate (map rotateL) points

allRotationsAround :: Coord -> [Coord] -> [[Coord]]
allRotationsAround o points = take 4 $ iterate (map (rotateLAround o)) points

neighbours :: Coord -> [Coord]
neighbours p = [p + (1, 0), p + (0, 1), p + (-1, 0), p + (0, -1)]

neighbours8 :: Coord -> [Coord]
neighbours8 p = [
        p + (1, 0),
        p + (1, 1),
        p + (0, 1),
        p + (-1, 1),
        p + (-1, 0),
        p + (-1, -1),
        p + (0, -1),
        p + (1, -1)
    ]

neighboursIn :: (Coord, Coord) -> Coord -> [Coord]
neighboursIn range = filter (inRange range) . neighbours

parseCoord :: C.ByteString -> Coord
parseCoord str = let
        [a, b] = C.split ',' str
        Just (x, _) = C.readInt a
        Just (y, _) = C.readInt b
    in (x, y)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

manhattanRange :: Int -> Coord -> [Coord]
manhattanRange d p = [p' | p' <- range (p - (d, d), p + (d, d)), manhattan p p' <= d]

--data Direction = North | East | South | West
data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    deriving (Show, Eq, Ord)

rotateDirR dir = case dir of
    North -> East
    East -> South
    South -> West
    West -> North
    _ -> undefined

rotateDirL dir = case dir of
    North -> West
    East -> North
    South -> East
    West -> South
    _ -> undefined

reflectDir dir = case dir of
    North -> South
    East -> West
    South -> North
    West -> East
    _ -> undefined

isDirection :: Char -> Bool
isDirection c = c == '^' || c == '>' || c == 'v' || c == '<'

dirFromChar :: Char -> Direction
dirFromChar c = case c of
    '^' -> North
    '>' -> East
    'v' -> South
    '<' -> West
    _ -> undefined

dirVec :: (Num a, Num b) => Direction -> (a, b)
dirVec d = case d of
    North     -> ( 0, -1)
    NorthEast -> ( 1, -1)
    East      -> ( 1,  0)
    SouthEast -> ( 1,  1)
    South     -> ( 0,  1)
    SouthWest -> (-1,  1)
    West      -> (-1,  0)
    NorthWest -> (-1, -1)

vecDir :: Coord -> Direction
vecDir v = case v of
    ( 0, -1) -> North
    ( 1, -1) -> NorthEast
    ( 1,  0) -> East
    ( 1,  1) -> SouthEast
    ( 0,  1) -> South
    (-1,  1) -> SouthWest
    (-1,  0) -> West
    (-1, -1) -> NorthWest
    _ -> error ("not a 'unit' vector: " ++ show v)

step :: Coord -> Direction -> Coord
step p d = p + dirVec d

walk :: Coord -> [Direction] -> Coord
walk = foldl' step

walkAll :: Coord -> [Direction] -> [Coord]
walkAll start dirs = reverse $ snd $ foldl' walkAcc (start, []) dirs where
    walkAcc :: (Coord, [Coord]) -> Direction -> (Coord, [Coord])
    walkAcc (p, ps) d = (step p d, step p d:ps)

type DirIterator = ((Int, Int), (Int, Int)) -> [[(Int, Int)]]

iterateEast :: DirIterator
iterateEast bounds = [[(x, y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
    where ((xmin, ymin), (xmax, ymax)) = bounds

iterateWest :: DirIterator
iterateWest bounds = [[(x, y) | x <- reverse [xmin..xmax]] | y <- [ymin..ymax]]
    where ((xmin, ymin), (xmax, ymax)) = bounds

iterateNorth :: DirIterator
iterateNorth bounds = [[(x, y) | y <- reverse [ymin..ymax]] | x <- [xmin..xmax]]
    where ((xmin, ymin), (xmax, ymax)) = bounds

iterateSouth :: DirIterator
iterateSouth bounds = [[(x, y) | y <- [ymin..ymax]] | x <- [xmin..xmax]]
    where ((xmin, ymin), (xmax, ymax)) = bounds

iterateNorthEast :: DirIterator
iterateNorthEast bounds = [
        [(max i (i + slice - h), min h slice - i) | i <- [0..min sideMin $ min slice (slices - slice)]
    ] | slice <- [0..slices]]
    where ((xmin, ymin), (xmax, ymax)) = bounds
          w = xmax - xmin
          h = ymax - ymin
          slices = w + h
          sideMin = min w h

iterateNorthWest :: DirIterator
iterateNorthWest bounds = [
        [(min (slices - slice) w - i, min h slice - i) | i <- [0..min sideMin $ min slice (slices - slice)]
    ] | slice <- [0..slices]]
    where ((xmin, ymin), (xmax, ymax)) = bounds
          w = xmax - xmin
          h = ymax - ymin
          slices = w + h
          sideMin = min w h

iterateSouthWest :: DirIterator
iterateSouthWest bounds = [
        [(min w slice - i, max 0 (slice - w) + i) | i <- [0..min sideMin $ min slice (slices - slice)]
    ] | slice <- [0..slices]]
    where ((xmin, ymin), (xmax, ymax)) = bounds
          w = xmax - xmin
          h = ymax - ymin
          slices = w + h
          sideMin = min w h

iterateSouthEast :: DirIterator
iterateSouthEast bounds = [
        [(max 0 (slice - h) + i, max 0 (min h slices - slice) + i) | i <- [0..min sideMin $ min slice (slices - slice)]
    ] | slice <- [0..slices]]
    where ((xmin, ymin), (xmax, ymax)) = bounds
          w = xmax - xmin
          h = ymax - ymin
          slices = w + h
          sideMin = min w h

printPointSet :: Coord -> Set.Set Coord -> String
printPointSet (width, height) points =
    concat [if x == width then [charAt (x, y), '\n'] else [charAt (x, y)] | (x, y) <- coords] `debug` show (range ((0, 0), (width, height)))
    where coords = range ((0, 0), (width, height))
          charAt :: Coord -> Char
          charAt pos = if Set.member pos points then 'X' else '.'

printPoints :: Coord -> [Coord] -> String
printPoints (width, height) points =
    concat [if x == width then [charAt (x, y), '\n'] else [charAt (x, y)] | y <- [0..height], x <- [0..width]]
    where charAt :: Coord -> Char
          charAt pos = if pos `elem` points then 'X' else '.'
