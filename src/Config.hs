{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.ByteString
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)

data HSResumeBuilderPreferences = HSResumeBuilderPreferences
  { personalInformation :: HSResumeBuilderPersonalInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data HSResumeBuilderPersonalInfo = HSResumeBuilderPersonalInfo
  { contactLines :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

getConfig :: IO (Maybe HSResumeBuilderPreferences)
getConfig = do
  Data.Yaml.decodeFileThrow ".hsresumebuilder.yaml" :: IO (Maybe HSResumeBuilderPreferences)