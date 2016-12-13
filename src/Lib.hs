{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)
import Data.List (isPrefixOf)
import Hanabi.Types

data Request
  = ConnectionRequest Text
  | DiscardCardRequest Card
  | HintColorRequest Text
                     Color
  | HintNumberRequest Text
                      Number
  | PlayCardRequest Card
  | GameStartRequest
  deriving (Show, Generic)

data Response
  = ErrorResponse Text
  | ConnectionResponse
  | DiscardCardResponse
  | PlayCardResponse
  | HintColorResponse
  | GameOver Score
  deriving (Show, Generic)

newtype Score =
  Score Int
  deriving (Show, Eq, Generic)

data Error =
  Error
  deriving (Show, Generic)

instance ToJSON Number

instance ToJSON Color

instance ToJSON Hint

instance ToJSON Card where
  toEncoding = Aeson.genericToEncoding requestOptions

instance ToJSON Request where
  toEncoding = Aeson.genericToEncoding requestOptions

instance ToJSON Error

instance ToJSON Score

instance ToJSON Response where
  toEncoding = Aeson.genericToEncoding responseOptions

instance FromJSON Number

instance FromJSON Color

instance FromJSON Hint

instance FromJSON Card where
  parseJSON = Aeson.genericParseJSON requestOptions

instance FromJSON Request where
  parseJSON = Aeson.genericParseJSON requestOptions

instance FromJSON Error

instance FromJSON Score

instance FromJSON Response where
  parseJSON = Aeson.genericParseJSON responseOptions

requestOptions :: Aeson.Options
requestOptions =
  Aeson.defaultOptions
  { Aeson.fieldLabelModifier = labelMod
  , Aeson.constructorTagModifier = typeMod
  , Aeson.sumEncoding = encoding "req_type"
  }

responseOptions =
  Aeson.defaultOptions
  { Aeson.fieldLabelModifier = labelMod
  , Aeson.constructorTagModifier = typeMod
  , Aeson.sumEncoding = encoding "res_type"
  }

encoding :: String -> Aeson.SumEncoding
encoding s =
  Aeson.TaggedObject
  { Aeson.tagFieldName = s
  , Aeson.contentsFieldName = "payload"
  }

labelMod :: String -> String
labelMod s
  | "_" `isPrefixOf` s = drop 1 s
  | otherwise = s

typeMod :: String -> String
typeMod "Connect" = "CONNECTION_REQUEST"
typeMod "Discard" = "DISCARD_CARD_REQUEST"
typeMod "HintColor" = "HINT_COLOR_REQUEST"
typeMod "HintNumber" = "HINT_NUMBER_REQUEST"
typeMod "Play" = "PLAY_CARD_REQUEST"
typeMod s = s
