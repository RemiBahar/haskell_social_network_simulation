module Main where
import Control.Concurrent
--For parallisation

import qualified Control.Parallel.Strategies as CPS 
import qualified Control.DeepSeq

import qualified Data.Sequence as DS -- For implementing sequences for [User]
import qualified Data.Foldable as DF

import Functions 
import Types

main :: IO ()
main = do
    -- Get user input
    putStrLn "Please enter how many users to simulate (default - 10)"
    users_input <- getLine
    let total_users = validateInput users_input 10

    putStrLn "Please enter how many messages to simulate (default - 100)"
    messages_input <- getLine
    let total_messages = validateInput messages_input 100

    putStrLn $ "Running simulation with " ++ show total_users ++ " users sending " ++ show total_messages ++ " messages in total."

    -- Initialize MVars
    let user_sequence = DS.fromList $ CPS.parMap CPS.rdeepseq createUser [1..total_users]
    --let user_sequence = DS.fromList $ map createUser [1..total_users]

    total <- newMVar 0
    winner <- newEmptyMVar
    users <- newMVar user_sequence

    -- Generate user threads
    mapFork users winner total [1..total_users] total_messages total_users

    -- Won't execute until all messages are sent when winner MVar is not empty. Ensures threads aren't killed early.
    w <- takeMVar winner
    
    -- Output number of messages
    u <- takeMVar users
    let uList = DF.toList u
    let output = (CPS.parMap CPS.rdeepseq) outputStatsPar uList
    mapM_ print output