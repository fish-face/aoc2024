{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bits
import GHC.Data.Maybe

import Text.Printf
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import qualified Data.ByteString as B
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
--    deriving (Show)k
--    prog :: Vector Int,

instance {-# OVERLAPPING #-} Show (Maybe Int) where
    show Nothing = "---"
    show (Just x) = printf "%03b" x

main :: IO ()
main = do
    input <- readInputLines
    let
        regALine = input !! 0
        regBLine = input !! 1
        regCLine = input !! 2
        progLine = input !! 4

        a = parseReg regALine
        b = parseReg regBLine
        c = parseReg regCLine

        prog = parseProg progLine
        part1 = execute prog (State 0 a b c) []
--    print (a, b, c, prog)
    putStrLn $ C.unpack $ C.intercalate "," $ map (C.pack . show) part1
    print $ part2 prog

parseReg :: C.ByteString -> Int
parseReg = parseInt . C.drop 12

parseProg :: C.ByteString -> Vector Op
parseProg line = let
        relevant = C.drop 9 line
        digs = C.split ',' relevant
    in
    V.fromList $ map (parseInt) digs

parseInt = fst . fromJust . C.readInt

execute :: Vector Op -> State -> [Int] -> [Int]
execute prog state@(State p a b c) output = if p >= V.length prog then reverse output --`debug` (show state)
    else let
            (nextState, out) = step prog state
            nextOut = case out of
                Just (out') -> (out':output)
                Nothing -> output
        in execute prog nextState nextOut

step :: Vector Op -> State -> (State, Maybe Int)
step prog state@(State i a b c) = let
        op = prog V.! i
        opval = prog V.! (i + 1)
        comboval = getComboValue opval state
        stepPtr = i + 2 --`debug` (show (op, opval, comboval))
    in
--    swap $ traceShowId $ swap $
    if
        | op == 0 -> -- adv
            (state {iPtr = stepPtr, regA = (a `shiftR` comboval)}, Nothing)
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
            (state {iPtr = stepPtr, regB = (a `shiftR` comboval)}, Nothing)
        | op == 7 -> -- cdv
            (state {iPtr = stepPtr, regC = (a `shiftR` comboval)}, Nothing)
        | otherwise -> error ("invalid opcode at " ++ (show i))

getComboValue :: Int -> State -> Int
getComboValue v state@(State i a b c) = if
    | v == 0 -> v
    | v == 1 -> v
    | v == 2 -> v
    | v == 3 -> v
    | v == 4 -> a
    | v == 5 -> b
    | v == 6 -> c
    | otherwise -> error ("invalid combo code " ++ (show v))

printWord :: Int -> String
printWord x = printf "%03b" x

printWords :: Int -> String
printWords x = let bin = printf "%03b" x in
    reverse $ intercalate " " $ chunksOf 3 $ reverse bin

swap (a, b) = (b, a)

--part2 :: Vector Int -> Int
--part2 nums = let
--        bins = map bin $ V.toList nums :: [[Bool]]
--        flatBins = concat bins :: [Bool]
--        -- 0 <= i < 3
--        matches
--            word
--            flatBins
--            flip
--            i
--            shift = let
--                res = flip
--                    `xor` (flatBins !! i)
--                    `xor` (flatBins !! (i + shift)) == word !! i --`debug` (show (i, shift))
--            in
--            res --`debug` ("matches " ++ (show i) ++ " " ++ (show shift) ++ "? --> " ++ (show res))
--        createWord :: [Bool] -> [Bool] -> Int -> [Bool]
--        createWord [b0, b1, b2] flatBins shift = [
--                b0 `xor` flatBins !! shift,
--                b1 `xor` flatBins !! (1 + shift),
--                b2 `xor` flatBins !! (2 + shift)
--            ]
--        word2bin :: [Bool] -> Int
--        word2bin word = go word 0 where
--            go [] acc = acc
--            go (b:bs) acc = go bs $ (acc * 2) + (fromEnum b)
--        requiredShift word flatBins = find (\s -> (
--                   matches word flatBins True 0 s
--                && matches word flatBins False 1 s
--                && matches word flatBins False 2 s
--            )) [1..7] --`debug` ("word: " ++ show word ++ ", all: " ++ (show $ map (map fromEnum) bins))
--        requiredShifts :: [[Bool]] -> [Bool] -> [[Bool]]
--        requiredShifts [] _ = []
--        requiredShifts _ [] = []
--        requiredShifts (word:words) flatBins = let shift = requiredShift word flatBins in
--            case shift of
--                Just s -> (createWord word flatBins s:requiredShifts words (drop 3 flatBins)) `debug` (show $ map fromEnum flatBins)
--                Nothing -> ([[False, False, False, True, True, True]])
--    in
--    0 `debug` (show $ map (map fromEnum) $ requiredShifts bins flatBins)

part2 prog = let
        prog' = V.toList prog
    in
    fromJust $ go prog' 0 0 where
    go :: [Int] -> Int -> Int -> Maybe [Int]
    go [] _ _ = Just []
    go nums fixed fixedBits = let
--            remainingBits = 10 - fixedBits
--            vMin = max 64 (1 `shiftL` (fixedBits - 1))
            vMax = 4095 `shiftR` fixedBits :: Int
            as = map (\v -> (v `shiftL` fixedBits) + fixed) [0..vMax]
            -- run the program with a and return the output
            testExec a = execute prog (State 0 a 0 0) []
            -- does the program, run on a, output the first two numbers
    --        test nums a = (take 2 nums) == (take (min 2 (length nums)) $ testExec a) --`debug` ("targ: " ++ (show (take 2 nums)) ++ " a: " ++ (show a) ++ " res: " ++ (show (take 2 $ testExec a)))
            test nums a = (head nums) == (head $ testExec a) --`debug` ("targ: " ++ (show (head nums)) ++ " a: " ++ (show $ Just a) ++ " res: " ++ (show (head $ testExec a)))
--            as = [3773, 3543]
            recurse a = go (tail nums) ((a `shiftR` 3) .&. 4095) 9
            valid = filter (test nums) as :: [Int]
--            results = map (\v -> if isJust (recurse v) then Just (v .&. 7:(fromJust $ recurse v)) else Nothing) valid :: [Maybe [Int]]
            results = map (\v -> do { recursed <- recurse v; return (v .&. 7:recursed) } ) valid :: [Maybe [Int]]
        in
        firstJusts results
--        do
--            result <- find test as `debug` ("fixed " ++ (show fixed) ++ " = " ++ (show $ Just fixed))
--            recurse <- go (tail nums) ((result `shiftR` 3) .&. 7) 3 `debug` ("result: " ++ (show $ Just result) ++ " -> " ++ (show $ testExec result))
--            return (result:recurse)

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
