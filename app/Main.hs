module Main (main) where

import Lib
import Lib.Days
import System.Directory
import Control.Monad
import Data.Foldable
import System.Environment
import System.Exit

commaSeparated :: [String] -> String
commaSeparated = inner ""
    where
    inner :: String -> [String] -> String
    inner current [] = current
    inner current (last:[]) = current <> last
    inner current (next:rest) = inner (current <> next <> ", ") rest

getDay :: [String] -> (Either String DynDay)
getDay [] =
    Left $ ("No Days Provided, valid values are [" <> (commaSeparated . map name $ days) <> "]")
getDay (n:_) =
    case find (\it -> name it == n) days of
        Just day -> Right day
        Nothing -> Left $ n <> " is not a valid day task\nvalid values are [" <> (commaSeparated . map name $ days) <> "]"

-- |Print optional failure information, and the usage of the program
usage :: forall a. Maybe String -> IO a
usage Nothing = do
    programName <- getProgName
    putStrLn $ "Usage:"
    putStrLn $ programName <> " <day>"
    exitWith (ExitFailure 1)

usage (Just e) = do
    putStrLn e
    usage Nothing

main :: IO ()
main = do
    _ <- getCurrentDirectory >>= putStrLn
    res <- fmap getDay getArgs
    case res of
        Left e -> usage $ Just e
        Right day -> runDay day
