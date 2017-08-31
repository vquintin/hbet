module HBet.Types
  ( Country(..)
  , WinOrDraw(..)
  ) where

data Country
  = Belgium
  | Bulgaria
  | Brasil
  | Chile
  | Columbia
  | Croatia
  | England
  | France
  | Germany
  | Ireland
  | Italia
  | Japan
  | Mexico
  | Poland
  | Romania
  | Slovenia
  | Spain
  | USA
  deriving (Eq, Show)

data WinOrDraw
  = W1
  | Draw
  | W2
  deriving (Eq, Show)
