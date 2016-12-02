{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import GHC.Generics (Generic)
import Data.List (isPrefixOf)

import Hanabi.Types

data Request
  = Connect Text
  | Discard Card
  | HintColor Color
  | HintNumber Number
  | Play Card
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Number

instance ToJSON Color

instance ToJSON Card where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance ToJSON Request where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

instance FromJSON Number

instance FromJSON Color

instance FromJSON Card where
  parseJSON = Aeson.genericParseJSON customOptions

instance FromJSON Request where
  parseJSON = Aeson.genericParseJSON customOptions

customOptions :: Aeson.Options
customOptions =
  Aeson.defaultOptions
  { Aeson.fieldLabelModifier = labelMod
  , Aeson.constructorTagModifier = typeMod
  , Aeson.sumEncoding = typeEncoding
  }

typeEncoding :: Aeson.SumEncoding
typeEncoding =
  Aeson.TaggedObject
  { Aeson.tagFieldName = "type"
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
typeMod s = error ("request type " ++ s ++ "not recognized")
