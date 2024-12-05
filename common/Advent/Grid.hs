module Advent.Grid where

import Data.Array
import Data.List
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Coord

fromLines :: [C.ByteString] -> Array (Int, Int) Char
fromLines lines = listArray (linesBounds lines) (C.unpack $ C.concat $ C.transpose lines)

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

--withCoords :: [[a]] -> [(Int, Int), a]
withCoords lines = [((i, j), c) | (i, line) <- enumerate lines, (j, c) <- enumerate line]

linesBounds :: [C.ByteString] -> ((Int, Int), (Int, Int))
linesBounds lines = ((0, 0), (C.length (head lines) - 1, length lines - 1))

toString :: Array (Int, Int) Char -> String
toString grid = concat [if x == width then [grid ! (x, y), '\n'] else [grid ! (x, y)] | (x, y) <- coords]
    where (_, (_, width)) = bounds grid
          coords = concat $ iterateEast $ bounds grid
