module Advent.Input where

import qualified System.Environment as SE
import qualified Data.ByteString as B

readInput :: IO B.ByteString
readInput = do
    args <- SE.getArgs
    B.readFile (head args)

readInputLines :: IO [B.ByteString]
readInputLines = do
    args <- SE.getArgs
    lines <- B.split 10 <$> B.readFile (head args)
    return $ filter (not . B.null) lines
