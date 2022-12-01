import System.IO
import Control.Monad

main = do
    contents <- getFileLines "input.txt"
    let elves = (sumCallories . makeNumbers . splitOnEmpty) contents
    let result = greatest elves
    putStrLn $ "The Elf Carrying the most had " <> show result <> " Calories"
    let top3 = topN 3 elves
    putStrLn $ "The top three Elves had " <> show top3 <> ", adding up to " <> (show $ sum top3) <> " Calories"
    return ()

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


summarize :: [Int] -> [String]
summarize = map row . zip [1..]
    where
    row :: (Int, Int) -> String
    row (index, val) = "Elf " <> (show index) <> " has " <> (show val) <> " calories"

sumCallories :: [[Int]] -> [Int]
sumCallories = map (foldl (\current next -> current + next) 0)

makeNumbers :: [[String]] -> [[Int]]
makeNumbers sections = map (\section -> map read section) sections

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty lines = inner [] [] lines
    where
    inner :: [[String]] -> [String] -> [String] -> [[String]]
    inner out next [] =
        out ++ [next]
    inner out next ("":rest) =
        inner (out ++ [next]) [] rest
    inner out next (it:rest) =
        inner out (next ++ [it]) rest


-- | This is a function defined here?
printLines :: forall a. Show a => [a] -> IO ()
printLines lines = do
    forM lines (\value -> (putStrLn . show) value)
    return ()

-- | Get the contents of a file by lines
getFileLines path = do
    contents <- readFile path
    return (lines contents)
