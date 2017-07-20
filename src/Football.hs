{-# LANGUAGE MultiParamTypeClasses #-}
module Football
    (
    ) where
import Control.Arrow ((&&&))
import Numeric.Natural
import Lib (Scorable, toChoice)

data FootballScore = FootballScore
  { halfTime1 :: Natural
  , halfTime2 :: Natural
  , fullTime1 :: Natural
  , fullTime2 :: Natural
  }

{- For bets on the final score-}
data FootballCorrectScore = FootballCorrectScore
  { correctScore1 :: Natural
  , correctScore2 :: Natural
  } deriving (Eq, Show)

instance Scorable FootballScore FootballCorrectScore where
  toChoice fs = FootballCorrectScore (halfTime1 fs + fullTime1 fs)
                                     (halfTime2 fs + fullTime2 fs)

{- For bets on the match winner -}
data FootballFullTime = FT1 | FTDraw | FT2 deriving (Eq, Show)

instance Scorable FootballScore FootballFullTime where
  toChoice fs
    | s1 > s2 = FT1
    | s1 == s2 = FTDraw
    | otherwise = FT2
    where s1 = halfTime1 fs + fullTime1 fs
          s2 = halfTime2 fs + fullTime2 fs

{- For bets on the half time "winner" -}
data FootballHalfTime = HT1 | HTDraw | HT2 deriving (Eq, Show)

instance Scorable FootballScore FootballHalfTime where
  toChoice fs
    | s1 > s2 = HT1
    | s1 == s2 = HTDraw
    | otherwise = HT2
    where s1 = halfTime1 fs
          s2 = halfTime2 fs

{- For bets where which are a product of the first half time winner -}
newtype FootballHalfFullTime = FootballHalfFullTime (FootballHalfTime, FootballFullTime)
                               deriving (Eq, Show)

instance Scorable FootballScore FootballHalfFullTime where
  toChoice fs = FootballHalfFullTime (toChoice fs, toChoice fs)
