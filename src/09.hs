{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Debug.Trace

import Data.Maybe
import Data.List (inits, findIndex, find, sort)
--import Data.Vector
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Advent
import Advent.Input

data Block = Empty Int | File Int Int Int
    deriving (Show)

--instance Show Block where
--    show (Empty size) = cycleN size "."
--    show (File size val _) = cycleN size (show val)
--
--instance {-# OVERLAPPING #-} Show [Block] where
--    show blocks = concat $ map show blocks

isEmpty (Empty _) = True
isEmpty (File _ _ _) = False

isFile = not . isEmpty

bSize (Empty size) = size
bSize (File size _ _) = size

cycleN :: Int -> [a] -> [a]
cycleN _ [] = []
cycleN 0 _  = []
cycleN n xs = xs ++ cycleN (n-1) xs

-- (location, size), size: smallest loc of that size
type Space = Set (Int, Int)


main :: IO ()
main = do
    input <- readInput
    let
        stripped = C.strip input
        block idx val startloc = case idx `mod` 2 of
            0 -> File val (idx `div` 2) startloc
            1 -> Empty val
        id i = case i `mod` 2 of
            0 -> i `div` 2
            1 -> -1
        nums = map readCharInt $ C.unpack stripped
--        memory = V.fromList $ Prelude.concat $ [Prelude.replicate (readCharInt (input `C.index` i)) (id i) | i <- [0..(C.length input) - 1]]
        locations = map sum $ inits nums :: [Int]
--        blocks = zipWith (curry (uncurry (block))) [0..] nums
        makeblocksverygood = zip (zip [0..] nums) locations :: [((Int, Int), Int)]
        blocks = map (uncurry (uncurry (block))) makeblocksverygood :: [Block]
        enumeratedSizes = zip [0..] $ zip locations nums :: [(Int, (Int, Int))]
        emptyLocations = [(loc, size) | (i, (loc, size)) <- enumeratedSizes, i `mod` 2 == 1]
        spaces = Set.fromList emptyLocations
--    print locations
--    print spaces
--    print blocks
    print $ part1 blocks
--    print memory`
--    print $ part1old memory
    print $ part2 spaces (reverse blocks)

readCharInt :: Char -> Int
readCharInt c = fromEnum c - 48

part1 :: [Block] -> Int
part1 blocks = go (tail blocks) (reverse blocks) (bSize (head blocks)) (sum $ map bSize blocks) where
    go :: [Block] -> [Block] -> Int -> Int -> Int
    go [] [] _ _ = 0
    go (Empty eSize:blocks) (File fSize id _:rblocks) left right =
        --traceShow (Empty eSize:blocks, File fSize id:rblocks, left, right) $
        if
            | left >= right -> 0 `debug` ("terminating " ++ show (left, right)) -- TODO
            | eSize == fSize -> let
                    (skippedF:nextEmpty) = blocks
                    (skippedE:nextFiles) = rblocks
                    nextLeft = left + eSize + bSize skippedF
                    nextRight = right - fSize - bSize skippedE
--                    value = id * left + id * (left + 1) ... + id * (left + size)
                    value = checksumContrib (File fSize id left) + if nextLeft <= nextRight then (checksumContrib' (left + eSize) skippedF) else 0
                in
                value + go nextEmpty nextFiles nextLeft nextRight `debug` ("== " ++ (show (value, checksumContrib (File fSize id left), checksumContrib' (left + eSize) skippedF)))
            | eSize > fSize -> let
                    (skippedE:nextFiles) = rblocks
                    nextLeft = left + fSize
                    nextRight = right - fSize - bSize skippedE
                    value = checksumContrib (File fSize id left)
                in
                value + go (Empty (eSize - fSize):blocks) nextFiles nextLeft nextRight `debug` ("> " ++ (show value))
            | otherwise -> let -- eSize < fSize
                    (skippedF:nextEmpties) = blocks
                    nextLeft = left + eSize + bSize skippedF
                    nextRight = right - eSize
                    (File skippedSize skippedId undefined) = skippedF
                    effectiveSize = min (nextRight - left - eSize) skippedSize
                    value = checksumContrib (File eSize id left) + (checksumContrib (File effectiveSize skippedId (left + eSize)))
                    --`debug` (show (checksumContrib left (File eSize id),(checksumContrib (left + eSize) skippedF)))
                in
                value + go nextEmpties (File (fSize - eSize) id undefined:rblocks) nextLeft nextRight `debug` ("< " ++ (show value))
    go a b c d = error $ show (a, b, c, d)

checksumContrib :: Block -> Int
checkusmContrib (Empty _) = error "bad"
checksumContrib (File size id loc) = size * (2 * id * loc + (size - 1) * id) `div` 2

checksumContrib' :: Int -> Block -> Int
checkusmContrib' _ (Empty _) = error "bad"
checksumContrib' idx (File size id _) = size * (2 * id * idx + (size - 1) * id) `div` 2

part2 :: Space -> [Block] -> Int
part2 spaces [] = 0
part2 spaces ((Empty _):blocks) = part2 spaces blocks
part2 spaces ((File size id startloc):blocks) = let
--        maybeTarget = Set.lookupGE size spaces
        maybeTarget = findSpace spaces size
    in
--    trace ("considering file " ++ (show (File size id startloc)) ++ " with space:\n" ++ (show spaces)) (
    case maybeTarget of
        Just (targetLoc, targetSize) ->
            if targetLoc < startloc then
                -- moving file to some location with targetSize gap
                let
                    -- remove at old size
                    removedSpace = Set.delete (targetLoc, targetSize) spaces
                    nextSpaces = if targetSize == size then
                            -- exactly filling space
                            removedSpace
                        else
                            -- extra space
                            let newSize = targetSize - size
                                newLoc = targetLoc + size
                            in
                            -- add at new size
                            Set.insert (newLoc, newSize) removedSpace
                in
                checksumContrib (File size id targetLoc) + (part2 nextSpaces blocks) --`debug` ("  moving to " ++ show targetLoc)
            else
                -- not moving file
                checksumContrib (File size id startloc) + (part2 spaces blocks) --`debug` "  leaving (space -->)"
        Nothing -> checksumContrib (File size id startloc) + (part2 spaces blocks) --`debug` "  leaving"
--        )
part2 a b = error $ show (a, b)

-- searches `spaces` for a key >= the given integer
findSpace :: Space -> Int -> Maybe (Int, Int)
findSpace spaces req = find (\(_, size) -> size >= req) $ Set.elems spaces
--find (\(i, found) -> found && (not $ null (spaces Map.! i))) [(i, Map.member i spaces)| i<-[req..9]]

cons x xs = (x:xs)
