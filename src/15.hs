{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List
import Data.Maybe
import Data.Array ((!), (//), assocs, bounds)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Search as Search

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
        part1 = runPart1 arena start instructions
    print $ score part1
    let
        -- what fresh fucking hell is this
        rep1 = C.toStrict $ Search.replace "#" ("##" :: C.ByteString) gridPart :: C.ByteString
        rep2 = C.toStrict $ Search.replace "." (".." :: C.ByteString) rep1 :: C.ByteString
        rep3 = C.toStrict $ Search.replace "O" ("[]" :: C.ByteString) rep2 :: C.ByteString
        rep4 = C.toStrict $ Search.replace "@" ("@." :: C.ByteString) rep3 :: C.ByteString
        gridLines2 = C.lines rep4
        start2 = findStart gridLines2
        arena2 = fromLines gridLines2
        part2 = runPart2 arena2 start2 instructions
    print $ score2 part2

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
    in runPart1 nextGrid nextPos ds

score :: Grid Char -> Int
score grid = sum $ map (scoreCrate) cratePositions where
    cratePositions = map (fst) $ filter (\(p, c) -> c == 'O') $ assocs grid
    scoreCrate (x, y) = x + 100 * y

apply2 :: Grid Char -> Coord -> Direction -> (Grid Char, Coord)
apply2 grid p d = let
        (moved, newGrid, newP) = go grid p d where
            go grid p d = let
                    atPos = grid ! p
                    newP = step p d
                    atNew = grid ! newP
                    alter = [(newP, atPos), (p, '.')]
                in
                case atNew of
                    '#' -> (False, grid, p)
                    '.' -> (True, grid // alter, newP)
                    '[' -> if
                        | d `elem` [East, West] -> let
                                (moved, next, _) = go grid newP d
                            in
                            if moved then (True, next // alter, newP)
                            else (False, grid, p)
                        | d `elem` [North, South] -> let
                                (movedL, nextL, _) = go grid newP d
                                newPR = newP + (1, 0)
                                atR = grid ! (p + (1, 0))
                                (movedR, nextR, _) = go nextL newPR d
                            in
                            if movedL && movedR then (True, nextR // alter, newP)
                            else (False, grid, p)
                    ']' -> if
                        | d `elem` [East, West] -> let
                                (moved, next, _) = go grid newP d
                            in
                            if moved then (True, next // alter, newP)
                            else (False, grid, p)
                        | d `elem` [North, South] -> let
                                (movedR, nextR, _) = go grid newP d
                                newPL = newP - (1, 0)
                                atL = grid ! (p - (1, 0))
                                (movedL, nextL, _) = go nextR newPL d
                            in
                            if movedL && movedR then (True, nextL // alter, newP)
                            else (False, grid, p)
    in
--    (newGrid // [(p, '.')], newP)
    (newGrid, newP)

runPart2 :: Grid Char -> Coord -> [Direction] -> Grid Char
runPart2 grid _ [] = grid
runPart2 grid p (d:ds) = let
        (nextGrid, nextPos) = apply2 grid p d
    in runPart2 nextGrid nextPos ds

score2 :: Grid Char -> Int
score2 grid = sum $ map (scoreCrate) cratePositions where
    cratePositions = map (fst) $ filter (\(p, c) -> c == '[') $ assocs grid
    scoreCrate (x, y) = x + 100 * y
