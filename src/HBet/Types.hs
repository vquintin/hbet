module HBet.Types
  ( Country(..)
  , WinOrDraw(..)
  ) where

data Country
  = Belgium
  | Bulgaria
  | Chile
  | Croatia
  | France
  | Germany
  | Ireland
  | Japan
  | Mexico
  | Poland
  | Slovenia
  | Romania
  deriving (Eq, Show)

data WinOrDraw
  = W1
  | Draw
  | W2
  deriving (Eq, Show)
