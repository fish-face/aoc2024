{-# LANGUAGE FlexibleInstances #-}

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
import Data.PQueue.Min

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

type Space = MinQueue (Int, Int)

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
        enumeratedSizes = zip [0..] $ zip nums locations :: [(Int, (Int, Int))]
        blankLocations = Map.fromList [(size, []) | size <- [0..9]]
        emptyLocations = [(size, [loc]) | (i, (size, loc)) <- enumeratedSizes, i `mod` 2 == 1]
--        withLocations = zip locations blocks
--        emptyBlocks = filter (\(_, b) -> isEmpty b) withLocations
--        emptySizes = map (\(_, b) -> bSize b) emptyBlocks
--        bySize = zip emptySizes $ map (\x -> [x]) emptyBlocks
        spaces' = Map.fromListWith (++) $ reverse emptyLocations
        spaces = Map.unionWith (++) blankLocations spaces'
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
        if left >= right then 0 --`debug` ("terminating " ++ show (left, right)) -- TODO
        else
            if eSize == fSize then
                let
                    (skippedF:nextEmpty) = blocks
                    (skippedE:nextFiles) = rblocks
                    nextLeft = left + eSize + bSize skippedF
                    nextRight = right - fSize - bSize skippedE
--                    value = id * left + id * (left + 1) ... + id * (left + size)
                    value = checksumContrib (File fSize id left) + (checksumContrib' (left + eSize) skippedF)
                in
                value + go nextEmpty nextFiles nextLeft nextRight --`debug` show value
            else
                if eSize > fSize then
                    let
                        (skippedE:nextFiles) = rblocks
                        nextLeft = left + fSize
                        nextRight = right - fSize - bSize skippedE
                        value = checksumContrib (File fSize id left)
                    in
                    value + go (Empty (eSize - fSize):blocks) nextFiles nextLeft nextRight --`debug` show value
                else -- eSize < fSize
                    let
                        (skippedF:nextEmpties) = blocks
                        nextLeft = left + eSize + bSize skippedF
                        nextRight = right - eSize
                        (File skippedSize skippedId undefined) = skippedF
                        effectiveSize = min (nextRight - left - eSize) skippedSize
                        value = checksumContrib (File eSize id left) + (checksumContrib (File effectiveSize skippedId (left + eSize)))
                        --`debug` (show (checksumContrib left (File eSize id),(checksumContrib (left + eSize) skippedF)))
                    in
                    value + go nextEmpties (File (fSize - eSize) id undefined:rblocks) nextLeft nextRight --`debug` show value
    go a b c d = error $ show (a, b, c, d)

checksumContrib :: Block -> Int
checkusmContrib (Empty _) = error "bad"
checksumContrib (File size id loc) = size * (2 * id * loc + (size - 1) * id) `div` 2

checksumContrib' :: Int -> Block -> Int
checkusmContrib' _ (Empty _) = error "bad"
checksumContrib' idx (File size id _) = size * (2 * id * idx + (size - 1) * id) `div` 2

part1old :: V.Vector Int -> Int
part1old memory = let
        Just firstFree = V.findIndex (== (-1)) memory
        Just lastOcc = V.findIndexR (/= (-1)) memory
        (defragged, final) = defragp1 memory firstFree lastOcc --`debug` (show (firstFree, lastOcc))
        enumerated = Prelude.zip (V.toList (V.take (final + 2) defragged)) [0..]
        muls = Prelude.map (uncurry (*)) enumerated
    in
    Prelude.sum $ muls --`debug` (show muls)

defragp1 :: V.Vector Int -> Int -> Int -> (V.Vector Int, Int)
defragp1 memory firstFree lastOcc =
    if firstFree >= lastOcc then (memory, lastOcc)
    else let
            newmem = (memory V.// [(firstFree, memory V.! lastOcc), (lastOcc, -1)])
            Just newFF' = V.findIndex (== (-1)) (V.drop (firstFree + 1) memory)
            newFF = firstFree + 1 + newFF'
            Just newLO = V.findIndexR (/= (-1)) (V.take lastOcc memory)
        in defragp1 newmem newFF newLO --`debug` (show newmem)

part2 :: Map Int [Int] -> [Block] -> Int
part2 spaces [] = 0
part2 spaces ((Empty _):blocks) = part2 spaces blocks
part2 spaces ((File size id startloc):blocks) = let
        maybeTarget = findSpace spaces size
    in
    trace ("considering file " ++ (show (File size id startloc)) ++ " with space:\n" ++ (show spaces)) (
    case maybeTarget of
        Just (targetSize, _) -> let targetLoc = head (spaces Map.! targetSize) :: Int in
            if targetLoc < startloc then
                -- moving file to some location with targetSize gap
                let
                    -- remove at old size
                    removedSpace = Map.adjust tail targetSize spaces
                    nextSpaces = if targetSize == size then
                            -- exactly filling space
                            removedSpace
                        else
                            -- extra space
                            let newSize = targetSize - size
                                newLoc = targetLoc + size
                            in
                            -- add at new size
                            Map.adjust (sort . cons newLoc) newSize removedSpace
                in
                checksumContrib (File size id targetLoc) + (part2 nextSpaces blocks) `debug` ("  moving to " ++ show targetLoc)
            else
                -- not moving file
                checksumContrib (File size id startloc) + (part2 spaces blocks) `debug` "  leaving (space -->)"
        Nothing -> checksumContrib (File size id startloc) + (part2 spaces blocks) `debug` "  leaving"
        )
part2 a b = error $ show (a, b)

-- searches `spaces` for a key >= the given integer
findSpace :: Map Int [Int] -> Int -> Maybe (Int, Bool)
findSpace spaces req = find (\(i, found) -> found && (not $ null (spaces Map.! i))) [(i, Map.member i spaces)| i<-[req..9]]

cons x xs = (x:xs)
