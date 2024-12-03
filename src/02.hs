module Main where

import qualified System.Environment as SE

main :: IO ()
main = do
    input <- readInputLines
    let parsed = map parse input
    print $ sum $ map (fromEnum . safe) parsed
    print $ sum $ map (fromEnum . part2) parsed

readInputLines :: IO [String]
readInputLines = do
    args <- SE.getArgs
    lines <$> readFile (head args)

parse :: String -> [Int]
parse s = map read $ words s

safe :: [Int] -> Bool
safe xs = let ds = diffs xs in
    and [ 1 <= d && d <= 3 | d <- ds ] || and [ -3 <= d && d <= -1 | d <- ds ]

diffs :: Num a => [a] -> [a]
diffs [] = []
diffs xs = [b - a | (a, b) <- zip xs $ tail xs]

part2 :: [Int] -> Bool
part2 xs = let xd = diffs xs in
    allBetween 1 3 (tail xd) || allBetween (-3) (-1) (tail xd) || allButOneBetween 1 3 xd Nothing || allButOneBetween (-3) (-1) xd Nothing

allButOneBetween :: Int -> Int -> [Int] -> Maybe Int -> Bool
allButOneBetween _ _ [] _ = True
allButOneBetween _ _ [_] _ = True
allButOneBetween lo hi (a:rest) prev =
    if between lo hi a then allButOneBetween lo hi rest (Just a)
    else all (between lo hi) ((head rest + a):tail rest) || (
        case prev of
            Just prev' -> all (between lo hi) ((prev' + a):rest)
            Nothing -> False
    )

allBetween :: (Foldable t, Ord a) => a -> a -> t a -> Bool
allBetween lo hi = all (between lo hi)

between :: Ord a => a -> a -> a -> Bool
between lo hi a = lo <= a && a <= hi
