{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.List

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import Data.Algorithm.MaximalCliques

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInputLines
    let
        pairs = map splitLine input
        nodes = listNodes pairs
        graph = makeGraph pairs
        trips = triples graph nodes
        cliques = getMaximalCliques (curry (graph !)) nodes
    print $ length $ filter (\(a, b, c) -> C.head a == 't' || C.head b == 't' || C.head c == 't') trips
    C.putStrLn $ C.intercalate "," $ sort $ snd $ maximum $ zip (map length cliques) cliques

splitLine :: C.ByteString -> (C.ByteString, C.ByteString)
splitLine line = let parts = C.split '-' line in (parts !! 0, parts !! 1)

listNodes :: Eq a => [(a, a)] -> [a]
listNodes edges = nub $ concatMap (\(a, b) -> [a, b]) edges

type Graph a = Map a [a]

makeGraph :: Ord a => [(a, a)] -> Graph a
makeGraph edges = M.fromListWith (++) $ map (\(a, b) -> (a, [b])) $ edges ++ map swap edges

(!) :: Ord a => Graph a -> (a, a) -> Bool
(!) graph (a, b) = b `elem` (graph M.! a)

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)

triples :: Ord a => Graph a -> [a] -> [(a, a, a)]
triples graph nodes = let --map (\v -> let
        adj1 = map (\v -> (v, graph M.! v)) nodes -- :: [(a, [a])]
        adj2 = concatMap (\(v1, v2s) -> map (\v2 -> (v1, v2, graph M.! v2)) v2s) adj1 -- :: [(a, a, [a])]
        adj3 = concatMap (\(v1, v2, v3s) -> map (\v3 -> (v1, v2, v3)) v3s) adj2 -- :: [(a, a, a)]
        trips = filter (\(v1, _, v3) -> graph ! (v1, v3)) adj3
    in unique $ map sortTriple $ filter uniqueTriple trips

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

sortTriple :: Ord c => (c, c, c) -> (c, c, c)
sortTriple (a, b, c) = let [a1, b1, c1] = sort [a, b, c] in (a1, b1, c1)

uniqueTriple :: Eq a => (a, a, a) -> Bool
uniqueTriple (a, b, c) = a /= b && b /= c && a /= c
