{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( someFunc
    ) where
import Control.Arrow ((&&&))
import Numeric.Natural

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Sport = Football

newtype Bet a = Bet
  { betChoices :: [Choice a]
  } deriving (Eq, Show)

data Choice a = Choice
  { choiceValue :: a
  , choiceOdd :: Double
  } deriving (Eq, Show)

class (Eq c) => Scorable s c where
  toChoice :: s -> c
  filterScores :: c -> [s] -> [s]
  filterScores c = filter (\s -> c == toChoice s)

data FootballScore = FootballScore
  { halfTime1 :: Natural
  , halfTime2 :: Natural
  , fullTime1 :: Natural
  , fullTime2 :: Natural
  }

data FootballCorrectScore = FootballCorrectScore
  { correctScore1 :: Natural
  , correctScore2 :: Natural
  } deriving (Eq, Show)

instance Scorable FootballScore FootballCorrectScore where
  toChoice = toCorrectScore

toCorrectScore :: FootballScore -> FootballCorrectScore
toCorrectScore fs =
  FootballCorrectScore (halfTime1 fs + fullTime1 fs)
                       (halfTime2 fs + fullTime2 fs)

data FootballFullTime = FT1 | FTDraw | FT2

toMr3 :: FootballScore -> FootballFullTime
toMr3 fs
  | s1 > s2 = FT1
  | s1 == s2 = FTDraw
  | otherwise = FT2
  where s1 = halfTime1 fs + fullTime1 fs
        s2 = halfTime2 fs + fullTime2 fs

data FootballHalfTime = HT1 | HTDraw | HT2

toHalfTime :: FootballScore -> FootballHalfTime
toHalfTime fs
  | s1 > s2 = HT1
  | s1 == s2 = HTDraw
  | otherwise = HT2
  where s1 = halfTime1 fs
        s2 = halfTime2 fs

newtype FootballHalfFullTime = FootballHalfFullTime (FootballHalfTime, FootballFullTime)

toHalfFullTime :: FootballScore -> FootballHalfFullTime
toHalfFullTime s = FootballHalfFullTime (toHalfTime s, toMr3 s)
