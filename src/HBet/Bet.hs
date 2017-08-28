{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HBet.Bet
  ( Choice(..)
  , Sport(..)
  , BetResult(..)
  , winOrLose
  , voidable
  ) where

class Sport a where
  data BetType a :: *
  data Competition a :: *
  data Lineup a :: *
  data Score a :: *
  generateScores :: [Score a]
  validate :: BetType a -> Score a -> BetResult

data Match a =
  Match (Competition a)
        (Lineup a)

deriving instance
         (Eq (Competition a), Eq (Lineup a)) => Eq (Match a)

deriving instance
         (Show (Competition a), Show (Lineup a)) => Show (Match a)

data Choice sport idty = Choice
  { tag :: idty
  , eventInfo :: Match sport
  , betType :: BetType sport
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
