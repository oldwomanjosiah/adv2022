module Main (main) where

import Lib
import System.Directory

main :: IO ()
main = do
    _ <- getCurrentDirectory >>= putStrLn
    Lib.day2
