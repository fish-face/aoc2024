module Advent where

import Debug.Trace
import Data.Bits (Bits(xor))
import Data.Maybe
import qualified Data.ByteString.Char8 as C

debug :: c -> String -> c
debug = flip trace

debugIt :: Show a => a -> a
debugIt x = trace (show x) x

unwrap :: Maybe a -> a
unwrap = fromJust

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt
