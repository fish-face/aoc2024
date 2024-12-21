{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Search

import Data.MemoUgly

import Advent
import Advent.Input

main :: IO ()
main = do
    input <- readInput
    let
        (partsLine, patternsLines) = C.breakSubstring "\n\n" input
        parts = map C.unpack $ split ", " partsLine
        patterns = map C.unpack $ C.lines $ C.strip patternsLines
        possibilities = map (numMatches parts) patterns
    print $ length $ filter (/= 0) possibilities
    print $ sum possibilities

numMatches :: [String] -> String -> Int
numMatches = curry $ memo $ uncurry inner
    where
        inner parts "" = 1
        inner parts s = sum $ map (numMatches parts) $ catMaybes $ map (\p -> stripPrefix p s) parts
