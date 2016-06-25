import Control.Concurrent
import Test.QuickCheck

---- DATA TYPES

-- Yes, cleaning staff is a new genre ;)
data Genre = Man | Woman | Clean deriving Show
data Person = Person Genre (IO ())
type Time = Int

---- HELPER FUNCTIONS

-- Helper function to represent seconds as microseconds
asMicro :: Int -> Int
asMicro n = n * (10 ^ (6 :: Int))

---- GENERATOR FUNCTIONS

-- Generator for the person genre with problem-set probablities
genGenre :: Gen Genre
genGenre = frequency [(49, return Man), (49, return Woman), (2, return Clean)]

-- Generator for the random time that a person stays in the bathroom, in microseconds
-- Range chosen is between 1 and 3 seconds
genTime :: Gen Time
genTime = choose (asMicro 1, asMicro 3)

-- Generator for the people entering the bathroom
genPerson :: Gen Person
genPerson = do
  genre <- genGenre
  time <- genTime
  return $ Person genre $ threadDelay time

---- MAIN PROGRAM

main :: IO ()
main = return ()
