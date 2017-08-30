{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HBet.Football
  ( Football(..)
  , Score(..)
  , BetType(..)
  , Competition(..)
  , Lineup(..)
  ) where

import Control.Arrow ((&&&))
import Data.Text (Text)
import HBet.Bet
import qualified HBet.Types as T

data Football

instance Sport Football where
  data Score Football = FootballScore{halfTime1 :: Int,
                                    halfTime2 :: Int, fullTime1 :: Int, fullTime2 :: Int}
                    deriving (Eq, Show)
  data BetType Football = HalfTimeWinOrDraw T.WinOrDraw
                      | FullTimeWinOrDraw T.WinOrDraw
                      | HTFTWinOrDraw T.WinOrDraw T.WinOrDraw
                      | Handicap Int T.WinOrDraw
                      | ExactScore Int Int
                      | DoubleChance T.WinOrDraw T.WinOrDraw
                      | NumberOfGoals Ordering Double
                      deriving (Eq, Show)
  data Competition Football = ChD1 T.Country
                          | ChD2 T.Country
                          deriving (Eq, Show)
  data Lineup Football = Lineup Text Text
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
  validate = undefined
