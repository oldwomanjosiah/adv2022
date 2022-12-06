module Day5 (Task(..)) where

import Lib.Days
import Data.Char
import Data.Maybe

data Task = Task deriving(Show)
data Crate = Crate Char deriving(Show)
data Move = Move { count :: Int, from :: Int, to :: Int } deriving(Show)

class FromStr t where
    fromStr :: String -> Either String (t, String)

instance FromStr Crate where
    fromStr ('[':t:']':rest) = Right (Crate t, rest)
    fromStr r = Left r

unCrate :: Crate -> Char
unCrate (Crate c) = c

crateRow :: String -> [Maybe Crate]
crateRow = map (fmap first . rightOrNothing . fromStr) . groupCt 4

toStacks :: [[Maybe Crate]] -> [[Crate]]
toStacks = inner []
    where
    inner :: [[Crate]] -> [[Maybe Crate]] -> [[Crate]]
    inner current rows | not $ all isEmpty rows =
        inner (current ++ [[x | (Just x:_) <- rows]]) $ map tail rows
    inner current _ = current

isEmpty :: forall a. [a] -> Bool
isEmpty [] = True
isEmpty _ = False

groupCt :: Int -> String -> [String]
groupCt _ [] = []
groupCt c i = take c i : (groupCt c $ drop c i)

rightOrNothing :: forall a b. Either a b -> Maybe b
rightOrNothing (Left _) = Nothing
rightOrNothing (Right it) = Just it

first :: forall a b. (a, b) -> a
first (it, _) = it

instance FromStr Move where
    fromStr i = let
        i1 = takeStr "move" i
        (count, i2) = takeInt $ dropWhile isSpace i1
        i3 = takeStr "from" $ dropWhile isSpace i2
        (from, i4) = takeInt $ dropWhile isSpace i3
        i5 = takeStr "to" $ dropWhile isSpace i4
        (to, rest) = takeInt $ dropWhile isSpace i5
        in Right (Move { count, from, to }, rest)

instance Day Task where
    type Result1 Task = String
    type Result2 Task = String

    name _ = "day5"
    index _ = 5

    part1 _ a = map (fromMaybe ' ' . fmap unCrate . listToMaybe)
        <$> uncurry (foldl (flip $ doMove True))
        <$> makeInput
        <$> input a

    part2 _ a = map (fromMaybe ' ' . fmap unCrate . listToMaybe)
        <$> uncurry (foldl (flip $ doMove False))
        <$> makeInput
        <$> input a


doMove :: Bool -> Move -> [[Crate]] -> [[Crate]]
doMove doRev Move { count, from, to } tower =
    uncurry (put to) $ get count from (tower)
    where
    rev :: [Crate] -> [Crate]
    rev = if doRev then reverse else id

    put :: Int -> [Crate] -> [[Crate]] -> [[Crate]]
    put _ _ [] = []
    put 1 toPut (h:rest) = (rev toPut ++ h) : rest
    put n toPut (h:rest) = h : put (n - 1) toPut rest

    get :: Int -> Int -> [[Crate]] -> ([Crate], [[Crate]])
    get _ _ [] = ([], [])
    get c 1 (h:rest) = (take c h, drop c h : rest)
    get c n (h:rest) = let (a, b) = get c (n - 1) rest in (a, h:b)
    

makeInput :: [String] -> ([[Crate]], [Move])
makeInput i = let
    (towers, moves) = splitInput i
    in (toStacks $ map crateRow $ towers, map (fst . unwrap . fromStr) moves)

unwrap :: forall a b. Either a b -> b
unwrap (Left _) = error $ "Expected Right b but got Left a"
unwrap (Right it) = it

splitInput :: [String] -> ([String], [String])
splitInput i = let (a, b) = break (== mempty) i in (a, dropWhile (== mempty) b)

takeStr :: String -> String -> String
takeStr expected i = let
        (tag, rest) = splitAt (length  expected) i
    in
        if tag /= expected then error $ "Expected to get \"" <> expected <> "\" but got \"" <> tag <> "\""
        else rest

takeInt :: String -> (Int, String)
takeInt i = let
    digits = takeWhile isDigit i
    rest = dropWhile isDigit i
    in (read digits, rest)
