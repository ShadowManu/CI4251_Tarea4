import Locks
import Queue

import Control.Monad (forever)
---- MAIN PROGRAM

main :: IO ()
main = do
  queue <- newQueue
  locks <- newLocks
  forever $ decreaseQueue queue locks
