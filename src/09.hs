module Main where

import Debug.Trace

import Data.Vector
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInput
    let
        id i = case i `mod` 2 of
            0 -> i `div` 2
            1 -> -1
        memory = fromList $ Prelude.concat $ [Prelude.replicate (readCharInt (input `C.index` i)) (id i) | i <- [0..(C.length input) - 1]] :: Vector Int
    print memory
    print $ part1 memory

readCharInt :: Char -> Int
readCharInt c = (fromEnum c) - 48

part1 :: Vector Int -> Int
part1 memory = let
        Just firstFree = findIndex (== (-1)) memory
        Just lastOcc = findIndexR (/= (-1)) memory
        (defragged, final) = defragp1 memory firstFree lastOcc `debug` (show (firstFree, lastOcc))
        enumerated = Prelude.zip (toList (slice 0 (final + 1) defragged)) [0..]
        muls = Prelude.map (uncurry (*)) enumerated
    in
    Prelude.sum $ muls `debug` (show muls)

defragp1 :: Vector Int -> Int -> Int -> (Vector Int, Int)
defragp1 memory firstFree lastOcc =
    if firstFree >= lastOcc then (memory, lastOcc)
    else let
            newmem = (memory // [(firstFree, memory V.! lastOcc), (lastOcc, -1)])
            Just newFF' = findIndex (== (-1)) (slice (firstFree + 1) 1000 memory)
            newFF = firstFree + 1 + newFF'
            Just newLO = findIndexR (/= (-1)) (slice 0 lastOcc memory)
        in defragp1 newmem newFF newLO --`debug` (show (memory, firstFree, lastOcc))
