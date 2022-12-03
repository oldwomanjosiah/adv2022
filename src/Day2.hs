module Day2(day2) where

import Lib.Util
import Data.Data
import Control.Exception
import Control.Applicative

-- newtype Rock = Rock ()
-- newtype Paper = Paper ()
-- newtype Scissors = Scissors ()

data Play = Rock | Paper | Scissors
    deriving(Eq, Ord)

data Strategy = Win | Lose | Draw
    deriving(Eq, Ord, Show)

type RoundStrat = (Play, Strategy)
type Round = (Play, Play)
type Score = Int


instance Show Play where
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

newtype InvalidPlay = InvalidPlay String
    deriving(Typeable)

instance Show InvalidPlay where
    show (InvalidPlay play) = play <> " is not valid input"


instance Exception InvalidPlay

class FromStr out where
    parse :: String -> Either InvalidPlay out

instance FromStr Play where
    parse "A" = Right Rock
    parse "X" = Right Rock
    parse "B" = Right Paper
    parse "Y" = Right Paper
    parse "C" = Right Scissors
    parse "Z" = Right Scissors
    parse str = Left $ InvalidPlay str

instance FromStr Strategy where
    parse "X" = Right Lose
    parse "Y" = Right Draw
    parse "Z" = Right Win
    parse str = Left $ InvalidPlay str

day2 :: IO ()
day2 = do
    lines <- getFileLines "res/2.txt"
    let playBased = sum $ map roundScore $ unwrap $ makeStrat lines
    let outcomeBased = sum $ map roundScore $ makeResponses $ unwrap $ makeStrat lines

    putStrLn $ "Play Based:    " <> show playBased
    putStrLn $ "Outcome Based: " <> show outcomeBased
    return ()

roundScore :: Round -> Score
roundScore = liftA2 (+) (playScore . snd) $ outcomeScore . uncurry outcome

playScore :: Play -> Score
playScore Rock = 1
playScore Paper = 2
playScore Scissors = 3

outcomeScore :: Strategy -> Score
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

outcome :: Play -> Play -> Strategy
outcome theirs ours = let
        theirScore = playScore theirs
        ourScore = playScore ours
    in
        -- The score for the play that beats yours is always one less than your
        -- score. (mod 3, because there are three options)
        case (theirScore - ourScore) `mod` 3 of
            0 -> Draw
            1 -> Lose
            2 -> Win
            _ -> error "Impossible because mod 3"

makeResponses :: [RoundStrat] -> [Round]
makeResponses = map $ \(a, b) -> (a, makeResponse a b)

makeResponse :: Play -> Strategy -> Play
makeResponse a Draw = a
makeResponse a Lose = makeResponse (makeResponse a Win) Win
makeResponse Rock Win = Paper
makeResponse Paper Win = Scissors
makeResponse Scissors Win = Rock

unwrap :: forall a b. Exception a => Either a b -> b
unwrap (Left inner) = throw inner
unwrap (Right inner) = inner

makeStrat :: forall a b. (FromStr a, FromStr b) => [String] -> Either InvalidPlay [(a, b)]
makeStrat = mapM (\line ->
    let w = words line in do
        l <- parse (w !! 0)
        r <- parse (w !! 1)
        return (l, r))
