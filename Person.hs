{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT
Maintainer: manuelalejandropm@gmail.com
Type and Operations of a custom person datatype
-}

module Person
( Genre(Man, Woman, Clean)
, Person(Person)
, genre
, duration
, action
, genPerson
) where

import Control.Concurrent
import Test.QuickCheck

---- CONSTANTS

minSeconds :: Int
minSeconds = 1

maxSeconds :: Int
maxSeconds = 3

manWeight :: Int
manWeight = 49

womanWeight :: Int
womanWeight = 49

cleanWeight :: Int
cleanWeight = 2

---- DATA TYPES

data Genre = Man | Woman | Clean deriving (Eq, Show)
data Person = Person
  { genre :: Genre
  , duration :: Double
  , action :: IO () }

type Time = Int

---- HELPER FUNCTIONS

-- Helper function to represent seconds as microseconds
toMicro :: Int -> Int
toMicro n = n * (10 ^ (6 :: Int))

-- Helper function to represent microseconds as floating seconds
toSeconds :: Int -> Double
toSeconds n = fromIntegral n / (10 ^ (6 :: Int))

-- PERSON INSTANCES

instance Show Person where
  show p = "Person " ++ show (genre p) ++ " " ++ show (duration p)

---- PERSON ACTIONS

-- By default, a person just waits a specified time
defAction :: Time -> IO ()
defAction = threadDelay

---- GENERATOR FUNCTIONS

-- Generator for the person genre with problem-set probablities
genGenre :: Gen Genre
genGenre = frequency
  [ (manWeight, return Man)
  , (womanWeight, return Woman)
  , (cleanWeight, return Clean) ]

-- Generator for the random time that a person stays in the bathroom, in microseconds
-- Range chosen is between 1 and 3 seconds
genTime :: Gen Time
genTime = choose (toMicro minSeconds, toMicro maxSeconds)

-- Generator for the people entering the bathroom
genPerson :: Gen Person
genPerson = do
  g <- genGenre
  t <- genTime
  return $ Person g (toSeconds t) (defAction t)
