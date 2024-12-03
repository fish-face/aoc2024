module Advent.Input where

import qualified System.Environment as SE

readInput :: IO String
readInput = do
    args <- SE.getArgs
    readFile (head args)

readInputLines :: IO [String]
readInputLines = do
    args <- SE.getArgs
    lines <$> readFile (head args)
