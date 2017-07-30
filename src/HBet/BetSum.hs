module HBet.BetSum
  ( BetSum(..)
  ) where

import qualified HBet.Football as FB

data BetSum
  = FootballFullTime (FB.FootballBetInfo FB.FootballFullTime)
  | FootballHalfTime (FB.FootballBetInfo FB.FootballHalfTime)
  | FootballHalfFullTime (FB.FootballBetInfo FB.FootballHalfFullTime)
  | FootballCorrectScore (FB.FootballBetInfo FB.FootballCorrectScore)
  deriving (Show)
