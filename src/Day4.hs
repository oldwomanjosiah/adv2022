module Day4(Task(..)) where

import Lib.Days

data Task = Task

instance Day Task where
    type Result1 Task = Int
    type Result2 Task = Int

    name _ = "day4"
    index _ = 4

    part1 _ a = length . filter hasContaned . map fromInput <$> input a

    part2 _ a = length . filter hasOverlap . map fromInput <$> input a

-- | Check whether either is entirely contained by the other.
--   Assumes pairs are in sorted order
hasContaned :: Pair -> Bool
hasContaned (Pair (Range ls le) (Range rs re)) =
    (ls <= rs && le >= re) || (ls >= rs && le <= re)

-- | Check whether there is any overlap by checking if either starts within the
--   range of the other.
hasOverlap :: Pair -> Bool
hasOverlap (Pair (Range ls le) (Range rs re)) =
    (ls >= rs && ls <= re) || (rs >= ls && rs <= le)

class FromInput a where
    fromInput :: String -> a

data Range = Range Int Int

instance FromInput Range where
    fromInput i =
        let (left, right) = break (== '-') i
        in Range (read left) (read $ dropWhile (== '-') right)

data Pair = Pair Range Range

instance FromInput Pair where
    fromInput i =
        let (left, right) = break (== ',') i
        in Pair (fromInput left) (fromInput $ dropWhile (== ',') right)
