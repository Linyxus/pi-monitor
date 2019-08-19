-- | Types
{-# LANGUAGE DeriveGeneric #-}

module Service.SMS.Types where

import GHC.Generics
import FileDB.Table
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (getCurrentTime, UTCTime)

data Alive = Alive { lastSeen :: UTCTime } deriving (Show, Generic)
instance ToJSON Alive
instance FromJSON Alive
instance DataRecord Alive

data Message = Message { content :: String
                       , timestamp :: UTCTime } deriving (Show, Generic)
instance ToJSON Message
instance FromJSON Message
instance DataRecord Message
