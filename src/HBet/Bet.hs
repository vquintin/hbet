{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HBet.Bet
  ( Bettable(..)
  , Choice(..)
  , Score(..)
  , BetResult(..)
  ) where

class Bettable eventInfo where
  data Score eventInfo :: *
  generateScores :: [Score eventInfo]

data Choice eventInfo idty = Choice
  { tag :: idty
  , validate :: Score eventInfo -> BetResult
  , eventInfo :: eventInfo
  , choiceOdd :: Double
  }

instance (Eq idty) => Eq (Choice score idty) where
  a == b = tag a == tag b && choiceOdd a == choiceOdd b

data BetResult
  = Win
  | Void
  | Lose
