{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Day3(Task(..)) where

import Lib.Days
import Lib.Util
import Data.Char
import Data.Bits
import Data.List

data Task = Task

instance Day Task where
    type Result1 Task = Int
    type Result2 Task = Int

    name _ = "day3"
    index _ = 3

    part1 _ state = sum . map rucksackPriority . map newRucksack <$> input state

    part2 _ state = (sum . map itemPriority . map groupBadge . makeGroups 3 . map newRucksack) <$> input state

data Sack = Sack {
    left :: [Char],
    right :: [Char],
    leftBits :: Int,
    rightBits :: Int,
    allBits :: Int
} deriving(Show)

newRucksack :: [Char] -> Sack
newRucksack inventory = let
    (left, right) = split inventory
    leftBits = bits left
    rightBits = bits right
    allBits = leftBits .|. rightBits
    in Sack { left, right, leftBits, rightBits, allBits }
    where
    split :: [Char] -> ([Char], [Char])
    split both = let half = length both `div` 2 in (take half both, drop half both)

    bits :: [Char] -> Int
    bits = foldl (\current -> \item -> current .|. ord item) 0

rucksackPriority :: Sack -> Int
rucksackPriority Sack { left, right, leftBits, rightBits = _ } = inner left leftBits right
    where
    inner :: [Char] -> Int -> [Char] -> Int
    inner left leftBits (it:rest) = if it `bitElem` leftBits && it `elem` left then itemPriority it else inner left leftBits rest
    inner _ _ [] = 0

bitElem :: Char -> Int -> Bool
bitElem char bits = bits .&. ord char /= 0

contains :: Sack -> Char -> Bool
contains Sack { left, leftBits, right, rightBits, allBits } char =
    if not $ char `bitElem` allBits then False
    else if char `bitElem` leftBits && char `elem` left then True
    else if char `bitElem` rightBits && char `elem` right then True
    else False

groupBadge :: [Sack] -> Char
groupBadge [] = error "Cannot get badge for group with no sacks"
groupBadge (Sack {left, right}:rest) = (find (finder rest) left) `orElse` (find (finder rest) right) `unwrapOr` error "No Common Item Found"
    where
    finder :: [Sack] -> Char -> Bool
    finder sacks c = all (`contains` c) sacks

makeGroups :: Int -> [a] -> [[a]]
makeGroups _ [] = []
makeGroups groupSize input = take groupSize input : (makeGroups groupSize $ drop groupSize input)

-- | Get the priority for an item in a rucksack
itemPriority :: Char -> Int
itemPriority c = if c >= 'a' && c <= 'z' then
        ord c - ord 'a' + 1
    else if c >= 'A' && c <= 'Z' then
        ord c - ord 'A' + 27
    else
        error $ c : " is not a alphabetic character"

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just it) _ = Just it
orElse Nothing (Just it) = Just it
orElse Nothing Nothing = Nothing

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just it) _ = it
unwrapOr Nothing it = it
