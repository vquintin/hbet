module HBet.BetSum
  ( ChoiceSum(..)
  ) where

import qualified HBet.Bet as HB
import qualified HBet.Football as FB

newtype ChoiceSum idty =
  Football (HB.Choice FB.Football idty)
  deriving (Eq, Show)
