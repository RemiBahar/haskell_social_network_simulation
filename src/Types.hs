{-# LANGUAGE DeriveGeneric #-}

module Types (
    User (..),
    Message (..)
) where

import qualified GHC.Generics as GG
import qualified Data.Text as DT 
import qualified Control.Parallel.Strategies as CPS

{-|
    Record to store user information.
-}
data User = User {
    user_id :: Int, -- ^ Used to identify the user
    messages_received :: Int, -- ^ Keeps track of how many messages are received by the user
    user_messages :: [Message] -- ^ All messages sent by or to the user are stored here
} deriving (Show, GG.Generic)


{-|
    Record to store message information. This record is embedded in an array in the user record
-}
data Message = Message {
    from :: Int, -- ^ user_id of user who sent the message
    to :: Int, -- ^ user_id of user who receives the message
    content :: DT.Text -- ^ content of message, i.e. a random string
} deriving (Show, GG.Generic)

-- So we can use parMap on User and Message
instance CPS.NFData User
instance CPS.NFData Message
