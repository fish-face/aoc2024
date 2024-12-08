module Advent.Input where

import qualified System.Environment as SE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

readInput :: IO B.ByteString
readInput = do
    args <- SE.getArgs
    B.readFile (head args)

readInputLines :: IO [B.ByteString]
readInputLines = do
    args <- SE.getArgs
    input <- B.readFile (head args)
    let
        stripped = C.strip input
        lines = B.split 10 stripped
--    lines <- B.split 10 <$> stripped
    return lines
--    return $ filter (not . B.null) lines
