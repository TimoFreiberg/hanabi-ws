{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

data Request
  = ConnectionRequest Text
  | GameStartRequest
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Request where
  toJSON (ConnectionRequest t) =
    object ["type" .= ("CONNECTION_REQUEST" :: Text), "name" .= t]
