{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HBet.Football
  ( FootballCompetition(..)
  , FootballMatch(..)
  ) where

import Control.Arrow ((&&&))
import HBet.Bet (Bettable, Choice, Score, generateScores)
import Numeric.Natural

{- football event info -}
data FootballCompetition
  = WorldCup
  | PremierLeague
  | Ligue1
  | GermanBundesliga
  | LigaPrimera
  | ItalianSerieA
  | ChampionsLeague
  | Ligue2
  | DutchEredivisie
  | BelgianFirstDivisionA
  | SwissSuperLeague
  | EnglishChampionship
  | GermanBundesliga2
  | ItalianSerieB
  | PrimeiraLiga
  deriving (Eq, Show)

data FootballMatch = FootballMatch
  { footballTeam1 :: String
  , footballTeam2 :: String
  , footballEvent :: FootballCompetition
  } deriving (Eq, Show)

instance Bettable FootballMatch where
  data Score FootballMatch = FootballScore{halfTime1 :: Natural,
                                         halfTime2 :: Natural, fullTime1 :: Natural,
                                         fullTime2 :: Natural}
                         deriving (Eq, Show)
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
