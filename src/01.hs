module Main where

import qualified System.Environment as SE
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readInputLines
    let (left, right) = unzip [ parse line | line <- input]
        dist = sum [abs $ a - b | (a, b) <- zip (sort left) (sort right)]
        counts = Map.fromListWith (+) (map (\x -> (x, 1)) right)
    print dist
    print $ sum [ l * fromMaybe 0 (Map.lookup l counts) | l <- left ]

readInputLines :: IO [String]
readInputLines = do
    args <- SE.getArgs
    lines <$> readFile (head args)

parse :: String -> (Int, Int)
parse s = (a, b) where
    [a, b] = map read $ take 2 $ words s
