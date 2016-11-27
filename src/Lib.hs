{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

import Hanabi.Types

data Message
  = Connect Text
  | Discard Card
  | HintColor Color
  | HintNumber Number
  | Play Card
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Message where
  toJSON msg = object (msgType msg : payload msg)

payload
  :: KeyValue t
  => Message -> [t]
payload (Connect name) = ["name" .= name]

msgType
  :: KeyValue kv
  => Message -> kv
msgType msg = "type" .= getType msg
  where
    getType :: Message -> Text
    getType (Connect _) = "CONNECTION_REQUEST"
