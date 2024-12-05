module Advent where

import Debug.Trace
import Data.Bits (Bits(xor))

debug = flip trace

debugIt :: Show a => a -> a
debugIt x = trace (show x) x

unwrap (Just x) = x
unwrap Nothing = undefined
