module Advent.Grid where

import Data.Array
import Data.List
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Coord

type Grid a = Array Coord a

fromLines :: [C.ByteString] -> Grid Char
fromLines lines = listArray (linesBounds lines) (C.unpack $ C.concat $ C.transpose lines)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

--withCoords :: [[a]] -> [(Int, Int), a]
withCoords lines = [((i, j), c) | (i, line) <- enumerate lines, (j, c) <- enumerate line]

linesBounds :: [C.ByteString] -> ((Int, Int), (Int, Int))
linesBounds lines = ((0, 0), (C.length (head lines) - 1, length lines - 1))

toString :: Grid Char -> String
toString grid = concat [if x == width then [grid ! (x, y), '\n'] else [grid ! (x, y)] | (x, y) <- coords]
    where (_, (_, width)) = bounds grid
          coords = concat $ iterateEast $ bounds grid
