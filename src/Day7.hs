module Day7(Task(..)) where

import Lib.Days
import Data.HashMap.Strict(HashMap)
import Data.Maybe

data Task = Task

instance Day Task where
    type Result1 Task = Int
    type Result2 Task = Int

    name _ = "day7"
    index _ = 7

    part1 _ a = do
        l <- toRootEntry . map toRawEntry <$> input a
        return $ totalLessThan 100000 l

    part2 _ a = do
        l <- toRootEntry . map toRawEntry <$> input a
        let s = size l
        let needed = 30000000
        let max = 70000000

        let deleteAtLeast = needed - (max - s)

        return $ fromMaybe 0 $ smallestGreaterThan l deleteAtLeast

data Entry = File String Int | Dir String Int [Entry]
    deriving(Show)

data RawEntry =
      Command { cmd :: String, arg :: Maybe String }
    | DirListing String
    | FileListing String Int
    deriving(Show)

name :: Entry -> String
name (File n _) = n
name (Dir n _ _) = n

size :: Entry -> Int
size (File _ s) = s
size (Dir _ s _) = s

totalSize :: [Entry] -> Int
totalSize = foldl (+) 0 . map size

smallestGreaterThan :: Entry -> Int -> Maybe Int
smallestGreaterThan (File _ _) m = Nothing
smallestGreaterThan (Dir _ s e) m =
    if s < m then Nothing
    else
        Just $ foldl min s $ mapMaybe (`smallestGreaterThan` m) e


totalLessThan :: Int -> Entry -> Int
totalLessThan max (File _ s) = 0
totalLessThan max (Dir _ s e) = let
    children = foldl (+) 0 $ map (totalLessThan max) e
    self = if s < max then s else 0
    in children + self


tree :: Entry -> String
tree = inner 0
    where
    inner :: Int -> Entry -> String
    inner d (File n s) =
        replicate (d * 2) ' ' <> "- " <> n <> ": " <> show s <> "\n"
    inner d (Dir n s e) =
        replicate (d * 2) ' ' <> "- dir " <> n <> ": " <> show s <> "\n" <> foldMap (inner $ d + 1) e

toRootEntry :: [RawEntry] -> Entry
toRootEntry (Command "cd" (Just "/"):rest) =
    let (c, rem) = inner [] rest
    in
        case rem of
            [] -> Dir "/" (totalSize c) c
            e -> error $ "Did not consume buffer, left " <> show e
    where
    inner :: [Entry] -> [RawEntry] -> ([Entry], [RawEntry])
    inner current (Command "ls" Nothing:rest) =
        let
            (files, r) = takeLs rest
        in inner (current ++ files) r
    inner current (Command "cd" (Just ".."):rest) = (current, rest)
    inner current (Command "cd" (Just n):rest) =
        let (c, r) = inner [] rest in
            inner (current ++ [Dir n (totalSize c) c]) r
    inner current (unknown:r) =
        error $ "Unexpected Item " <> show unknown <> " while getting " <> show current <> " from " <> show r
    inner current [] = (current, [])


    takeLs :: [RawEntry] -> ([Entry], [RawEntry])
    takeLs (DirListing _:r) = takeLs r
    takeLs (FileListing n s:r) =
        let (files, rem) = takeLs r in (File n s : files, rem)
    takeLs e = ([], e)

toRootEntry c = error $ "Entries list must start with `cd /` but got " <> show c

toRawEntry :: String -> RawEntry
toRawEntry = inner . words
    where
    inner :: [String] -> RawEntry
    inner ["$", cmd, arg] = Command { cmd = cmd, arg = Just arg }
    inner ["$", cmd] = Command { cmd = cmd, arg = Nothing }
    inner ["dir", n] = DirListing n
    inner [size, n] = FileListing n (read size)
    inner e = error $ show e <> " is not a known format"
