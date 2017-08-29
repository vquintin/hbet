{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HBet.Bet
  ( Choice(..)
  , Sport(..)
  , Match(..)
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

deriving instance
         (Eq (Match sport), Eq (BetType sport), Eq idty) =>
         Eq (Choice sport idty)

deriving instance
         (Show (Match sport), Show (BetType sport), Show idty) =>
         Show (Choice sport idty)

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
