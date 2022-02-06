module Functions
    ( 
      isNumeric,
      validateInput,
      createUser,
      outputStats,
      outputStatsPar,
      randomUser,
      mapFork,
      process
    ) where
import Control.Concurrent 

import qualified Control.Monad as CM
import qualified Text.StringRandom as TS
import qualified Data.Text as DT
import qualified Data.Sequence as DS
import qualified System.Random as SR
import qualified Data.Char as DC
import Types

{-|
    Returns True if a string contains only numeric characters and False otherwise
-}
isNumeric :: [Char] -- ^ String to test
            -> Bool -- ^ Result
isNumeric = foldr ((&&) . DC.isDigit) True



{-|
    Used to validate user input to ensure they enter a valid Int.
-}
validateInput :: [Char] -- ^ User input
                    -> Int -- ^ Default value. Returns this value if user enters an invalid input
                    -> Int -- ^ Returned value
validateInput i d 
    | i == "" = d
    | isNumeric i && read i > 1 = read i -- If less than 2 users, cant send a message to another user
    | otherwise = d

{-|
    Given a user_id creates a user record with that user_id.
    This function is used to create our 10 users by mapping createUser to [1..10]
-}
createUser :: Int -- ^ user_id of user to be created
                -> User -- ^ created user
createUser id = User id 0 []


{-|
    Prints the number of messages received by a user.
    Mapping this function to the final list of users is used to produce a final count of the messages received by each user.
-}
outputStats :: User-- ^ user record to display stats for 
                -> IO () -- ^ used to print user stats
outputStats user = putStrLn $ "User " ++ show (user_id user) ++ " has " ++ show (messages_received  user) ++ " messages"


{-|
    Gets the number of messages received by the user. Used with parMap rdeepseq to generate a list of message counts which is then displayed
    with mapM_
-}

outputStatsPar :: User -- ^ Input user
                    -> String -- ^ Output
outputStatsPar user = "User " ++ show (user_id user) ++ " has " ++ show (messages_received  user) ++ " messages"
{-|
    Selects a random index corresponding to a user which is not equal to the input exclude_index.
    This function is used by a user to randomly pick another user to send a message to.
-}
randomUser :: Int -- ^ randomIndex will not pick an index equal to this number
            -> Int -- ^ Number of users we have 
            -> IO Int -- ^ index of chosen list elementstack

randomUser exclude_index list_length = do
    x <- SR.randomRIO (1, list_length)

    -- Avoid sending message from a user to themself
    if x == exclude_index then
        randomUser exclude_index list_length
    else do
        return x



{-|
    Used to generate a thread for each user. 
    Using this function an arbitary number of threads can easily be created, e.g. instead of 10 threads 1000 threads can be created by running mapFork .. [1..1000]
-}
mapFork :: MVar (DS.Seq User) -- ^ Used by threads to update/access users and messages as the simulation runs
            -> MVar Int -- ^ Initially empty. After the simulation is finished, this will be populated with the total messages sent and main will print this.
            -> MVar Int -- ^ Used by threads to update/access total messages sent
            -> [Int] -- ^ List of user id's to generate threads for
            -> Int -- ^ Number of messages to send
            -> Int -- ^ Number of users
            -> IO () -- ^ Required output by forkIO (used to implement concurrency)
mapFork users winner total user_threads max_messages max_user  = do
    CM.forM_ user_threads $ \n ->    -- for each number 0 to 10
        forkIO (process n users winner total max_messages max_user)


{-|
Used to simulate a user in a thread. Will check if messages have reached the limit, if so it will exit and initialize the winner MVar. Otherwise it will:
    
-}

-- |
-- * pick a random user using Data.Random.randomIO. This user is in users and will have a different user_id to thread_user_id
-- * create some random text using Text.StringRandom.stringRandomIO. Using regex this will be between 10-20 characters, and only lowercase letters.
-- * send message. recipient and sender will be updated by replacing their values in Seq User
-- * wait a random time defined by Data.Random.randomRIO
-- * Recursively call itself with updated MVars
process :: Int 
            -> MVar (DS.Seq User) -- ^ MVar of sequence of users
            -> MVar Int -- ^ MVar of number of messages sent
            -> MVar Int -- ^ Empty MVar which is filled after all messages are sent
            -> Int -- ^ Number of messages to send
            -> Int  -- ^ Number of users
            -> IO () -- ^ Required output by forkIO (used to implement concurrency)
process user_id users winner total max_messages max_user = do
    -- Initialize MVars
    t <- takeMVar total
    u <- takeMVar users
    
    if t == max_messages then do
        -- Update MVars that we need to access later
        putMVar users u
        putMVar winner user_id
        
    else do
        --Generate message
        to <- randomUser user_id max_user

        let regex = DT.pack "[a-z]{1,20}$"
        randStr <- TS.stringRandomIO regex -- Generate random text between 1 and 20 characters

        let message = Message {to=to, from=user_id, content=randStr}

        -- Update from_user
        let from_user = DS.index u (user_id - 1)
        let from_user' = from_user {user_messages = message : user_messages from_user}

        --Update to_user
        let to_user = DS.index u (to - 1)
        let to_user' = to_user {messages_received = messages_received to_user + 1, user_messages = message : user_messages to_user} 

        --Update users
        let u' = DS.update (user_id - 1) from_user' u
        let u'' = DS.update (to - 1) to_user' u'

        -- Update MVars
        let t' = t + 1
        putMVar total t'
        putMVar users u''

        -- Wait random time
        x <- SR.randomRIO (1,max_user * 10)
        threadDelay x
        process user_id users winner total max_messages max_user