module Lib.Days(runDay, AdventState(..), Day(..)) where

import Data.IORef
import Data.Kind


class AdventState a where
    input :: a -> IO [String]

data DayInput = DayInput {
    filename :: String,
    contents :: IORef (Maybe [String])
};

getOrReadContents :: DayInput -> IO [String]
getOrReadContents this = do
    maybeLines <- readIORef $ contents this
    case maybeLines of
        Just c -> return c
        Nothing -> do
            let n = filename this
            putStrLn $ "Getting Input " <> n
            c <- readFile $ n
            let l = lines c
            writeIORef (contents this) (Just l)
            return l

instance AdventState (DayInput) where
    input = getOrReadContents

class (Show (Result1 t), Show(Result2 t)) => Day t where
    type Result1 t
    type Result2 t

    name :: t -> String
    index :: t -> Int
    part1 :: forall a. AdventState a => t -> a -> IO (Result1 t)
    part2 :: forall a. AdventState a => t -> a -> IO (Result2 t)

runDay :: forall t. Day t => t -> IO ()
runDay (day) = do
    putStrLn $ "Starting day " <> name day

    contents <- newIORef Nothing
    let filename = "res/" <> (show $ index day) <> ".txt"

    let state = DayInput { filename, contents }

    res1 <- part1 day state
    res2 <- part2 day state

    putStrLn $ "Part 1 Result: " <> show res1
    putStrLn $ "Part 2 Result: " <> show res2

