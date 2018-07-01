{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Shakespeare.Twitter where

import qualified Data.Aeson as Aeson
import           Data.Map (Map)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Tweet = Tweet
  { author :: Text
  , text :: Text
  } deriving Generic
instance Aeson.ToJSON Tweet
instance Aeson.FromJSON Tweet

data Thread = Thread { tweets :: [Tweet] } deriving Generic
instance Aeson.ToJSON Thread
instance Aeson.FromJSON Thread

data Book = Book { threads :: [Thread] } deriving Generic
instance Aeson.ToJSON Book
instance Aeson.FromJSON Book

data Config = Config
  { accounts :: Map Text TokenInfo
  , app :: AppInfo
  , tweetDelayMilliseconds :: Int
  } deriving Generic
instance Aeson.ToJSON Config
instance Aeson.FromJSON Config

data TokenInfo = TokenInfo
  { token :: Text
  , secret :: Text
  } deriving Generic
instance Aeson.ToJSON TokenInfo
instance Aeson.FromJSON TokenInfo

data AppInfo = AppInfo
  { key :: Text
  , secret :: Text
  } deriving Generic
instance Aeson.ToJSON AppInfo
instance Aeson.FromJSON AppInfo
