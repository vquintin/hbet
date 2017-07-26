{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
  ( Scorable
  , toChoice
  ) where

data Sport =
  Football

newtype Bet a = Bet
  { betChoices :: [Choice a]
  } deriving (Eq, Show)

data Choice a = Choice
  { choiceValue :: a
  , choiceOdd :: Double
  } deriving (Eq, Show)

class (Eq c) =>
      Scorable s c where
  toChoice :: s -> c
  filterScores :: c -> [s] -> [s]
  filterScores c = filter (\s -> c == toChoice s)
