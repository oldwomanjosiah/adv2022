module Day8(Task(..)) where

import Lib.Days
import Data.Char
import Data.List(transpose)

data Task = Task
type Forest = [[Int]]

instance Day Task where
    type Result1 Task = Int
    type Result2 Task = Int

    name _ = "day8"
    index _ = 8

    part1 _ a = (countVisible . allVisibility . newForrest) <$> input a
    part2 _ a = (foldl max 0 . map (foldl max 0) . allScores . newForrest) <$> input a

newForrest :: [String] -> Forest
newForrest [] = []
newForrest (hd:tl) = map digitToInt hd : newForrest tl

rowScores :: [Int] -> [Int]
rowScores = inner $ replicate 10 0
    where
    inner :: [Int] -> [Int] -> [Int]
    inner _ [] = []
    inner scores (hd:tl) =
        scores !! hd : (inner (map (+ 1) $ updateScores hd scores) tl)

    updateScores :: Int -> [Int] -> [Int]
    updateScores _ [] = error "Score map empty before index 0"
    updateScores 0 (_:tl) = 0 : tl
    updateScores e (_:tl) = 0 : updateScores (e - 1) tl

rowDualVisiblity :: [Int] -> [Bool]
rowDualVisiblity row = map (uncurry (||)) $ zip (rowVisibility row) $ reverse $ rowVisibility $ reverse row

allVisibility :: Forest -> [[Bool]]
allVisibility forest = map (map (uncurry (||))) zipped
    where
    rows = map rowDualVisiblity forest
    columns = transpose (map rowDualVisiblity (transpose forest))
    zipped = map (uncurry zip) (zip rows columns)

countVisible :: [[Bool]] -> Int
countVisible = sum . map (foldl (\curr it -> curr + if it then 1 else 0) 0)

rowVisibility :: [Int] -> [Bool]
rowVisibility = inner (-1)
    where
    inner :: Int -> [Int] -> [Bool]
    inner _ [] = []
    inner tallest (hd:tl) =
        let new = hd > tallest in
            new : inner (if new then hd else tallest) tl

rowDualScores :: [Int] -> [Int]
rowDualScores row = map (uncurry (*)) $ zip (rowScores row) $ reverse $ rowScores $ reverse row

allScores :: Forest -> [[Int]]
allScores forest = map (map (uncurry (*))) zipped
    where
    rows = map rowDualScores forest
    columns = transpose $ map rowDualScores $ transpose forest
    zipped = map (uncurry zip) (zip rows columns)
