{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lib
  ( Bet
  , choices
  , BetType
  , toChoice
  , filterScores
  , Choice
  , choiceValue
  , choiceOdd
  , Score
  , generateScores
  ) where

class Score score where
  generateScores :: [score]

class Bet betType score betInfo | betType -> score, betType score -> betInfo where
  choices :: betInfo -> [Choice betType]

class (Eq betType) =>
      BetType betType score | betType -> score where
  toChoice :: score -> betType
  filterScores :: betType -> [score] -> [score]
  filterScores bt = filter (\x -> bt == toChoice x)

data Choice a = Choice
  { choiceValue :: a
  , choiceOdd :: Double
  } deriving (Eq, Show)
