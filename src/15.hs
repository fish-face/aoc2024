{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List
import Data.Maybe
import Data.Array ((!), (//), assocs)
import Debug.Trace

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input
import Advent.Coord
import Advent.Grid

main :: IO ()
main = do
    input <- readInput
    let
        (gridPart, instructionPart) = C.breakSubstring (C.pack "\n\n") input
        gridLines = C.lines gridPart
        start = findStart gridLines
        arena = fromLines gridLines
        instructions = parseInstructions instructionPart
    let result = runPart1 arena start instructions
    putStr $ toString $ result
    print $ score result

findStart :: [C.ByteString] -> Coord
findStart gridLines = let
        nested = findIV isJust [C.findIndex (=='@') line|line <- gridLines]
        y = unwrap nested :: (Int, Maybe Int)
        x = unwrap $ snd y :: Int
    in
    (x, fst y)

findIV :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIV p = find (\(i, v) -> p v) . zip [0..]

parseInstructions :: C.ByteString -> [Direction]
parseInstructions instructions = map dirFromChar $ C.unpack $ C.filter (/='\n') instructions

apply :: Grid Char -> Coord -> Direction -> (Grid Char, Coord)
apply grid p d = let
        (moved, newGrid, newP) = go grid p d where
            go grid p d = let
                    atPos = grid ! p
                    newP = step p d
                    atNew = grid ! newP
                in
                case atNew of
                    '#' -> (False, grid, p)
                    '.' -> (True, grid // [(newP, atPos)], newP)
                    'O' -> let
                            (moved, next, _) = go grid newP d
                        in
                        if moved then (True, next // [(newP, atPos)], newP)
                        else (False, grid, p)
    in
    (newGrid // [(p, '.')], newP)

runPart1 :: Grid Char -> Coord -> [Direction] -> Grid Char
runPart1 grid _ [] = grid
runPart1 grid p (d:ds) = let
        (nextGrid, nextPos) = apply grid p d
    in runPart1 nextGrid nextPos ds --`debug` (toString grid)

score :: Grid Char -> Int
score grid = sum $ map (scoreCrate) cratePositions where
    cratePositions = map (fst) $ filter (\(p, c) -> c == 'O') $ assocs grid
    scoreCrate (x, y) = x + 100 * y
