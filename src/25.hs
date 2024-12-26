{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Graph as G
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input

type Key = [Int]
type Lock = [Int]

main :: IO ()
main = do
    input <- readInput
    let
        blocks = tokenise "\n\n" input
        (keys, locks) = foldl' parse ([], []) blocks
    print $ length [1 | key <- keys, lock <- locks, compatible key lock]

tokenise x y = h : if C.null t then [] else tokenise x (C.drop (C.length x) t)
    where (h,t) = C.breakSubstring x y

parse :: ([Key], [Lock]) -> C.ByteString -> ([Key], [Lock])
parse (keys, locks) block = let
        l1:rest = C.split '\n' block
    in case C.head l1 of
        '.' -> (parseKey rest:keys, locks)
        '#' -> (keys, parseLock rest:locks)

parseKey :: [C.ByteString] -> Key
parseKey rows = let
        cols = C.transpose rows
    in map (\c -> 5 - (fromJust $ C.findIndex (=='#') c)) cols

parseLock :: [C.ByteString] -> Key
parseLock rows = let
        cols = C.transpose rows
    in map (fromJust . C.findIndex (=='.')) cols

compatible :: Key -> Lock -> Bool
compatible k l = and $ map (\(a, b) -> a + b <= 5) $ zip k l
