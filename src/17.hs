{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.List.Split
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Bits

import Text.Printf

import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

type Op = Int

data State = State {
    iPtr :: Int,
    regA :: Int,
    regB :: Int,
    regC :: Int
}

instance Show State where
    show (State i a b c) = printf "%d\n\tA: %s\n\tB: %s\n\tC: %s" i (printWords a) (printWords b) (printWords c)

main :: IO ()
main = do
    input <- readInputLines
    let
        regALine = head input
        regBLine = input !! 1
        regCLine = input !! 2
        progLine = input !! 4

        a = parseReg regALine
        b = parseReg regBLine
        c = parseReg regCLine

        prog = parseProg progLine
        part1 = execute prog (State 0 a b c) []
    putStrLn $ C.unpack $ C.intercalate "," $ map (C.pack . show) part1
    print $ part2 prog

parseReg :: C.ByteString -> Int
parseReg = parseInt . C.drop 12

parseProg :: C.ByteString -> Vector Op
parseProg line = let
        relevant = C.drop 9 line
        digs = C.split ',' relevant
    in
    V.fromList $ map parseInt digs

parseInt :: C.ByteString -> Int
parseInt = fst . fromJust . C.readInt

execute :: Vector Op -> State -> [Int] -> [Int]
execute prog state@(State p a b c) output = if p >= V.length prog then reverse output --`debug` (show state)
    else let
            (nextState, out) = step prog state
            nextOut = case out of
                Just out' -> out':output
                Nothing -> output
        in execute prog nextState nextOut

step :: Vector Op -> State -> (State, Maybe Int)
step prog state@(State i a b c) = let
        op = prog V.! i
        opval = prog V.! (i + 1)
        comboval = getComboValue opval state
        stepPtr = i + 2 --`debug` (show (op, opval, comboval))
    in
    if
        | op == 0 -> -- adv
            (state {iPtr = stepPtr, regA = a `shiftR` comboval}, Nothing)
        | op == 1 -> -- bxl
            (state {iPtr = stepPtr, regB = b `xor` opval}, Nothing)
        | op == 2 -> -- bst
            (state {iPtr = stepPtr, regB = comboval `mod` 8}, Nothing)
        | op == 3 -> -- jnz
            if a == 0 then
                (state {iPtr = stepPtr}, Nothing)
            else
                (state {iPtr = opval}, Nothing)
        | op == 4 -> -- bxc
            (state {iPtr = stepPtr, regB = b `xor` c}, Nothing)
        | op == 5 -> -- out
            (state {iPtr = stepPtr}, Just (comboval `mod` 8))
        | op == 6 -> -- bdv
            (state {iPtr = stepPtr, regB = a `shiftR` comboval}, Nothing)
        | op == 7 -> -- cdv
            (state {iPtr = stepPtr, regC = a `shiftR` comboval}, Nothing)
        | otherwise -> error ("invalid opcode at " ++ show i)

getComboValue :: Int -> State -> Int
getComboValue v state@(State i a b c) = if
    | v == 0 -> v
    | v == 1 -> v
    | v == 2 -> v
    | v == 3 -> v
    | v == 4 -> a
    | v == 5 -> b
    | v == 6 -> c
    | otherwise -> error ("invalid combo code " ++ show v)

printWord :: Int -> String
printWord = printf "%03b"

printWords :: Int -> String
printWords x = let bin = printf "%03b" x in
    reverse $ unwords $ chunksOf 3 $ reverse bin

part2 :: Vector Op -> Int
part2 prog = let
        prog' = V.toList prog
        testExec a = execute prog (State 0 a 0 0) []
    in
    let res = go (reverse prog') 0 0 where
            go :: [Int] -> Int -> Int -> [Int]
            go [] _ fixed = [fixed]
            go (n:nums) i fixed = let
                    as = map (\v -> fixed `shiftL` 3 + v) [0..7]
                    -- run the program with a and return the output
                    test a = let
                            output = reverse (testExec a)
                        in length output > i && n == output !! i
                    valid = filter test as
                    results = concatMap recurse valid
                    recurse = go nums (i+1)
                in
                results
    in
    head res --`debug` show (zip (map int2words res) $ map testExec res)

concatMaybes :: [Maybe a] -> Maybe [a]
concatMaybes l = let filtered = catMaybes l in
    if null filtered then Nothing
    else Just filtered

bin :: Int -> [Bool]
bin n = map toEnum $ case n of
    0 -> [0, 0, 0]
    1 -> [0, 0, 1]
    2 -> [0, 1, 0]
    3 -> [0, 1, 1]
    4 -> [1, 0, 0]
    5 -> [1, 0, 1]
    6 -> [1, 1, 0]
    7 -> [1, 1, 1]

words2int :: [Int] -> Int
words2int = go 0 where
    go n [] = n
    go n (x:xs) = go (n `shiftL` 3 + x) xs

int2words :: Int -> [Int]
int2words 0 = []
int2words n = int2words (n `shiftR` 3) ++ [n .&. 7]
