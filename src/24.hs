{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Graph as G
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

data Gate = AND Label Label | OR Label Label | XOR Label Label | Const Bool
    deriving (Show)
type Label = C.ByteString
type Combination = (Gate, Label)

type Graph a = Map a [a]

makeGraph :: Ord a => [(a, a)] -> Graph a
makeGraph edges = M.fromListWith (++) $ map (\(a, b) -> (a, [b])) $ edges

(!) :: Ord a => Graph a -> (a, a) -> Bool
(!) graph (a, b) = b `elem` (graph M.! a)

outputs :: [Label]
outputs = ["z00", "z01", "z02", "z03", "z04", "z05", "z06", "z07", "z08", "z09", "z10", "z11", "z12", "z13", "z14", "z15", "z16", "z17", "z18", "z19", "z20", "z21", "z22", "z23", "z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31", "z32", "z33", "z34", "z35", "z36", "z37", "z38", "z39", "z40", "z41", "z42", "z43", "z44", "z45"]

main :: IO ()
main = do
    input <- readInput
    let
        (inputsPart, gatesPart) = C.breakSubstring "\n\n" input
        inputLines = C.lines inputsPart
        gateLines = C.lines $ C.strip gatesPart
        inputs = map parseInput inputLines
        gates = map parseGate gateLines
        -- because Data.Graph wants to take something that is *already* effectively an adjacency list, assemble
        -- it here (using makeGraph cribbed from d23) because `M.fromListWith` makes this easy
        myGraph = makeGraph $ concatMap (\(gate, output) -> case gate of
                AND a b -> [(a, output), (b, output)]
                OR  a b -> [(a, output), (b, output)]
                XOR a b -> [(a, output), (b, output)]
                Const _ -> []
            ) gates :: Graph Label
        -- now make the actual graph
        (graph, vertexLookup) = G.graphFromEdges' $ map (\(gate, label) -> (gate, label, fromMaybe [] (myGraph M.!? label) )) gates
        ordered = map ((\(a, b, _) -> (a, b)) . vertexLookup) (G.topSort graph)
        valueMap = foldl' eval (M.fromList $ map (\(Const v, l) -> (l, v)) inputs) ordered
    print $ foldl' (\total b -> (total `shiftL` 1) + b) 0 $ map (fromEnum . (valueMap M.!)) $ reverse outputs

parseInput :: C.ByteString -> Combination
parseInput line = let
        (name, value) = C.breakSubstring ": " line
    in (Const (toEnum $ fst $ fromJust $ C.readInt $ C.drop 2 value), name)

parseGate :: C.ByteString -> Combination
parseGate line = let
        (input, output) = C.breakSubstring " -> " line
        [a, gate, b] = C.words input
--        (a, rest) = C.break (== ' ') line
--        (gate, b) = C.break (== ' ')
        gate' = case gate of
            "AND" -> AND
            "OR" -> OR
            "XOR" -> XOR
            _ -> error ("unkown gate type " ++ (C.unpack gate))
    in (gate' a b, C.drop 4 output)

eval :: Map Label Bool -> Combination -> Map Label Bool
eval valueMap (gate, label) = let
        lookup a = valueMap M.! a
        newVal = case gate of
            AND a b -> lookup a && lookup b
            OR a b -> lookup a || lookup b
            XOR a b -> lookup a /= lookup b
            Const _ -> error "did not expect const"
    in M.insert label newVal valueMap
