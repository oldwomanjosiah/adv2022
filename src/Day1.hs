module Day1(task) where

import Lib.Util
import Lib.Days

newtype Day1Task = Day1Task ()

task :: Day1Task
task = Day1Task ()

instance Day Day1Task where
    type Result1 Day1Task = Int
    type Result2 Day1Task = Int

    name _ = "day1"
    index _ = 1

    part1 _ state = do
        l <- input state
        let elves = (sumCallories . makeNumbers . splitOnEmpty) l
        return $ greatest elves

    part2 _ state = do
        l <- input state
        let elves = (sumCallories . makeNumbers . splitOnEmpty) l
        return $ sum $ topN 3 elves

greatest :: [Int] -> Int
greatest = foldl (\l r -> if l > r then l else r) 0

-- | Find the greatest n values in array
topN :: Int -> [Int] -> [Int]
topN count elves = inner count (replicate count 0) elves
    where
    insert :: [Int] -> Int -> [Int]
    insert = insertInner []
        where
        insertInner :: [Int] -> [Int] -> Int -> [Int]
        insertInner l (next:r) ins =
            if ins > next
            then l ++ ins:next:r
            else insertInner (l ++ [next]) r ins
        insertInner l [] ins = l ++ [ins]

    inner :: Int -> [Int] -> [Int] -> [Int]
    inner count current (next:elves) =
        inner count (take count (insert current next)) elves
    inner _ current [] = current

sumCallories :: [[Int]] -> [Int]
sumCallories = map (foldl (\current next -> current + next) 0)

makeNumbers :: [[String]] -> [[Int]]
makeNumbers sections = map (\section -> map read section) sections

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = inner [] []
    where
    inner :: [[String]] -> [String] -> [String] -> [[String]]
    inner out next [] =
        out ++ [next]
    inner out next ("":rest) =
        inner (out ++ [next]) [] rest
    inner out next (it:rest) =
        inner out (next ++ [it]) rest

