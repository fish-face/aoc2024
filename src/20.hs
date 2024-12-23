{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.List
import qualified Data.Array as A

import Data.Map (Map)
import qualified Data.Map as M

import Advent
import Advent.Input
import Advent.Coord
import Advent.Grid

main :: IO ()
main = do
    input <- readInputLines
    let
        grid = fromLines input
        Just start = findElem grid 'S'
        Just end = findElem grid 'E'
        dist = mapPath grid start end
    print $ solve grid 100 dist 2
    print $ solve grid 100 dist 20

findElem :: Eq a => Grid a -> a -> Maybe Coord
findElem grid a = do
    found <- find (\(_, b) -> b == a) $ A.assocs grid
    return $ fst found

mapPath :: Grid Char -> Coord -> Coord -> Map Coord Int
mapPath grid start end = let
        bounds = A.bounds grid
        Just second = find (\p -> grid A.! p /= '#') $ neighboursIn bounds start
        go :: Coord -> Coord -> Int -> [(Coord, Int)]
        go prev cur len = ((cur, len):(
                if cur == end then []
                else let Just next = find (\p -> p /= prev && grid A.! p /= '#') $ neighboursIn bounds cur in
                    go cur next (len + 1)
            ))
    in
    M.fromList $ (start, 0):(go start second 1)

data Shortcut = Shortcut {
    sLen :: Int,
    sStart :: Coord,
    sEnd :: Coord
}
    deriving (Eq, Ord)
instance Show Shortcut where
    show (Shortcut l s e) = show l ++ ":" ++ show s ++ "->" ++ show e

solve :: Grid Char -> Int -> Map Coord Int -> Int -> Int
solve grid threshold pathMap shortcutLen = let
        bounds = A.bounds grid
        path = M.keys pathMap

        -- get all shortcuts starting at p
        getShortcuts :: Coord -> [Shortcut]
        getShortcuts p = filter valid $ map create $ manhattanRange shortcutLen p where
            valid (Shortcut _ _ e) = A.inRange bounds e && grid A.! e /= '#'
            create p1 = Shortcut (manhattan p p1) p p1

        -- what is the distance when taking this shortcut
        test :: Shortcut -> Int
        test (Shortcut l s e) = let
                distS = pathMap M.! s
                distE = pathMap M.! e
            in (distE - distS - l)
        shortcuts = concatMap getShortcuts path
        shortcutValues = map test shortcuts
    in
    length $ filter (>= threshold) shortcutValues
