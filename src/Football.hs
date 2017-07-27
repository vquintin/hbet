{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Football
  ( FootballScore
  , FootballCorrectScore
  , FootballFullTime
  , FootballHalfTime
  , FootballHalfFullTime
  ) where

import Control.Arrow ((&&&))
import Lib
       (Bet, BetType, Choice, Score, choices, generateScores, toChoice)
import Numeric.Natural

{- Types to represent football matches -}
data FootballEvent
  = WorldCup
  | PremierLeague
  | Ligue1
  | Bundesliga
  | LigaPrimera
  deriving (Eq, Show)

data FootballMatch = FootballMatch
  { footballTeam1 :: String
  , footballTeam2 :: String
  , footballEvent :: FootballEvent
  } deriving (Eq, Show)

data FootballScore = FootballScore
  { halfTime1 :: Natural
  , halfTime2 :: Natural
  , fullTime1 :: Natural
  , fullTime2 :: Natural
  } deriving (Eq, Show)

instance Score FootballScore where
  generateScores = fmap toScore tuples
    where
      toScore (a, b, c, d) = FootballScore a b c d
      tuples = concatMap f [0 ..]
      f n =
        [ (a, b, c, d)
        | a <- [0 .. n]
        , b <- [0 .. n]
        , c <- [0 .. n]
        , d <- [0 .. n]
        , a + b + c + d == n
        ]

data FootballBetInfo betType = FootballBetInfo
  { match :: FootballMatch
  , footballChoices :: [Choice betType]
  } deriving (Show)

instance (BetType betType FootballScore) =>
         Bet betType FootballScore (FootballBetInfo betType) where
  choices = footballChoices

{- For bets on the final score-}
data FootballCorrectScore = FootballCorrectScore
  { correctScore1 :: Natural
  , correctScore2 :: Natural
  } deriving (Eq, Show)

instance BetType FootballCorrectScore FootballScore where
  toChoice fs =
    FootballCorrectScore
      (halfTime1 fs + fullTime1 fs)
      (halfTime2 fs + fullTime2 fs)

{- For bets on the match winner -}
data FootballFullTime
  = FT1
  | FTDraw
  | FT2
  deriving (Eq, Show)

instance BetType FootballFullTime FootballScore where
  toChoice fs
    | s1 > s2 = FT1
    | s1 == s2 = FTDraw
    | otherwise = FT2
    where
      s1 = halfTime1 fs + fullTime1 fs
      s2 = halfTime2 fs + fullTime2 fs

{- For bets on the half time "winner" -}
data FootballHalfTime
  = HT1
  | HTDraw
  | HT2
  deriving (Eq, Show)

instance BetType FootballHalfTime FootballScore where
  toChoice fs
    | s1 > s2 = HT1
    | s1 == s2 = HTDraw
    | otherwise = HT2
    where
      s1 = halfTime1 fs
      s2 = halfTime2 fs

{- For bets where which are a product of the first half time winner -}
newtype FootballHalfFullTime =
  FootballHalfFullTime (FootballHalfTime, FootballFullTime)
  deriving (Eq, Show)

instance BetType FootballHalfFullTime FootballScore where
  toChoice fs = FootballHalfFullTime (toChoice fs, toChoice fs)
