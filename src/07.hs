module Main where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Conversion

import Advent
import Advent.Input

type Equation = (Int, [Int])

main :: IO ()
main = do
    input <- readInputLines
    let
        equations = map parse input
    print $ part1 equations
    print $ part2 equations

parse :: C.ByteString -> Equation
parse line = let
        (targetS, rem) = C.break (==':') line
        Just (target, _) = C.readInt targetS
        operands = map (fst . unwrap . C.readInt) $ C.words (C.drop 1 rem)
    in
    (target, operands)

part1 :: [Equation] -> Int
part1 eqs = sum $ map (\(t, ops) -> t * (fromEnum $ testp1 t (reverse ops))) eqs

testp1 :: Int -> [Int] -> Bool
testp1 target [x] = x == target
testp1 target (x:xs) = let
        addOutcome = (testp1 (target - x) xs) :: Bool
        mulOutcome = if x `divides` target then (testp1 (target `div` x) xs) else False :: Bool
    in
    addOutcome || mulOutcome

part2 :: [Equation] -> Int
part2 eqs = sum $ map (\(t, ops) -> t * (fromEnum $ testp2 t (reverse ops))) eqs

testp2 :: Int -> [Int] -> Bool
testp2 target [x] = x == target
testp2 target (x:xs) = let
        addOutcome = if x < target then (testp2 (target - x) xs) else False
        mulOutcome = if x `divides` target then (testp2 (target `div` x) xs) else False :: Bool
        catOutcome = let
                strx = C.pack $ show x :: C.ByteString
                strt = C.pack $ show target :: C.ByteString
                lenx = B.length strx
                lent = B.length strt
            in
            if lenx < lent && B.isSuffixOf strx strt then
                let res = C.readInt $ C.take (lent - lenx) strt in
                testp2 (fst $ unwrap $ res) xs
            else False
    in
    addOutcome || mulOutcome || catOutcome

divides a b = b `rem` a == 0
