{-# LANGUAGE DeriveGeneric #-}

module Shakespeare.Twitter where

import qualified Data.Aeson as Aeson
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
