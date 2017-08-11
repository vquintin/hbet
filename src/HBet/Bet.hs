{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HBet.Bet
  ( Bettable(..)
  , Choice(..)
  , Score(..)
  , BetResult(..)
  , winOrLose
  , voidable
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

winOrLose :: (a -> Bool) -> (a -> BetResult)
winOrLose prd = g . prd
  where
    g True = Win
    g False = Lose

voidable :: (a -> Bool) -> (a -> Bool) -> (a -> BetResult)
voidable win voided x =
  case (voided x, win x) of
    (True, _) -> Void
    (_, True) -> Win
    (_, False) -> Lose
