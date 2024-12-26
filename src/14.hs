{-# LANGUAGE OverloadedStrings, QuasiQuotes  #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Set as S
import Text.Regex.PCRE.Heavy
import qualified Data.ByteString.Char8 as C

import Advent
import Advent.Input
import Advent.Coord

type Robot = (Coord, Coord)

width = 101
height = 103
hx = 50
hy = 51

main :: IO ()
main = do
    input <- readInputLines
    let
        robots = map parse input
        steps = iterate (stepAll) robots
        after100 = steps !! 100
--    print robots
--    putStrLn $ printPoints (width-1, height-1) $ map (\(p, x) -> p) robots
--    putStrLn $ printPoints (width-1, height-1) $ map (\(p, x) -> p) after100
    print $ foldr (*) 1 $ countQuads after100
--    mapM_ print $ take 1000000 $ zip [0..] $ map output steps

parseregex = [re|p=([0-9]+),([0-9]+) v=([-0-9]+),([-0-9]+)|]

intGroups :: [(C.ByteString, [C.ByteString])] -> (Coord, Coord)
intGroups match = let
        results = head $ map (map (fst . unwrap . C.readInt) . snd) match
    in
    ((results !! 0, results !! 1), (results !! 2, results !! 3))

parse :: C.ByteString -> (Coord, Coord)
parse line = intGroups $ parseregex `scan` line

stepAll :: [Robot] -> [Robot]
stepAll robots = map (\(p, v) -> (mapXY ((flip mod) width) ((flip mod) height ) (p + v), v)) robots

applyN :: (a -> a) -> a -> Int -> a
applyN _ x 0 = x
applyN f x n = applyN f (f x) (n - 1)

countQuads :: [Robot] -> [Int]
countQuads robots = go robots (0, 0, 0, 0) where
    go [] (a, b, c, d) = [a, b, c, d]
    go (r:rs) (a, b, c, d) = go rs (case whichQuad r of
            0 -> (a+1, b, c, d)
            1 -> (a, b+1, c, d)
            2 -> (a, b, c+1, d)
            3 -> (a, b, c, d+1)
            _ -> (a, b, c, d)
        )
    whichQuad ((x, y), _) = if
        | x < hx, y < hy -> 0
        | x > hx, y < hy -> 1
        | x > hx, y > hy -> 2
        | x < hx, y > hy -> 3
        | otherwise -> -1

output :: [Robot] -> String
output robots = let
        points = S.fromList $ map fst robots
        coords = concat $ iterateEast ((0, 0), (width, height))
    in
    concat [[if S.member (x, y) points then '#' else '.'] | (x, y) <- coords]
