module Day6(Task(..)) where

import Lib.Days

data Task = Task

instance Day Task where
    type Result1 Task = Int
    type Result2 Task = Int

    name _ = "day6"
    index _ = 6

    part1 _ a =
        (length . fst . windowBreak 4 noneEqual . (!! 0)) <$> input a

    part2 _ a = 
        (length . fst . windowBreak 14 noneEqual . (!! 0)) <$> input a

windowBreak :: Int -> (String -> Bool) -> String -> (String, String)
windowBreak 0 _ = \inp -> (inp, "")
windowBreak count f = inner ""
    where
    inner :: String -> String -> (String, String)
    inner prev inp = if length inp < count then (prev, inp)
        else if f $ take count inp then (prev ++ take count inp, drop count inp)
        else inner (prev ++ take 1 inp) (drop 1 inp)

noneEqual :: forall a. Eq a => [a] -> Bool
noneEqual [] = True
noneEqual (hd:tl) = (all (/= hd) tl) && (noneEqual tl)
