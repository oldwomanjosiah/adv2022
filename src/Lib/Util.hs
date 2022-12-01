module Lib.Util(
    printLines,
    getFileLines
) where

import Control.Monad

-- | This is a function defined here?
printLines :: forall a. Show a => [a] -> IO ()
printLines l = do
    _ <- forM l (\value -> (putStrLn . show) value)
    return ()

-- | Get the contents of a file by lines
getFileLines :: FilePath -> IO [String]
getFileLines path = do
    contents <- readFile path
    return (lines contents)
