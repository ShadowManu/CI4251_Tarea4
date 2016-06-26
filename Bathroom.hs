import Control.Monad
import Control.Concurrent

import Test.QuickCheck

---- DATA TYPES

-- Yes, cleaning staff is a new genre ;)
data Genre = Man | Woman | Clean deriving Show
data Person = Person Genre (Locks -> IO ())
type Time = Int

type Queue = Chan Person

-- TODO THIS IS JUST PLACEHOLDER
data Locks = Locks
  { enter :: MVar ()
  , number :: MVar Int
  , available :: MVar () }

---- HELPER FUNCTIONS

-- Helper function to represent seconds as microseconds
asMicro :: Int -> Int
asMicro n = n * (10 ^ (6 :: Int))

-- LOCK FUNCTIONS

newLocks :: IO Locks
newLocks = do
  e <- newMVar ()
  n <- newMVar 0
  a<- newMVar ()

  return Locks { enter = e, number = n, available = a }

-- PERSON ACTIONS

manIO :: Locks -> Time -> IO ()
manIO lock time = do
  _ <- takeMVar $ enter lock -- Get ready to enter
  n <- takeMVar $ number lock -- Check the number of people inside

  if n < 3
    then do -- If room is available
      putMVar (number lock) (n+1) -- Say you're entering

    else do -- If not, wait for availability
      putMVar (number lock) n -- Put the number back
      takeMVar $ available lock -- Wait for someone to leave
      modifyMVar_ (number lock) $ \num -> return $ num+1 -- Say you're entering

  putMVar (enter lock) () -- Let others get ready to enter
  threadDelay time -- Clean your face and flush your bladder

  modifyMVar_ (number lock) $ \num -> return $ num-1 -- Say you're leaving
  _ <- tryPutMVar (available lock) ()-- Also let them know a slot is available

  putStrLn "I FINISHED BITCHES!"-- TODO JUST DEBUGGING
  return ()

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
  return $ Person genre $ \lock -> manIO lock time

---- QUEUE FUNCTIONS

initSizeQueue :: Int
initSizeQueue = 10

newQueue :: IO Queue
newQueue = do
  queue <- newChan
  replicateM_ initSizeQueue $ increaseQueue queue
  return queue

increaseQueue :: Queue -> IO ()
increaseQueue q = do
  pers <- generate genPerson
  case pers of
    -- Clean personnel is abusive
    p@(Person Clean _) -> unGetChan q p
    p                  -> writeChan q p

dequeueQueue :: Queue -> Locks -> IO ()
dequeueQueue q l = do
  person <- readChan q

  let process (Person _ action) = forkIO $ action l
  _ <- process person

  increaseQueue q
  dequeueQueue q l

---- MAIN PROGRAM

main :: IO ()
main = do
  queue <- newQueue
  locks <- newLocks
  forever $ dequeueQueue queue locks
