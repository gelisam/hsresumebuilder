{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.ByteString
import Data.Map (Map)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)

data Preferences = Preferences
  { personalInformation :: PersonalInfo,
    appearancePreferences :: AppearancePreferences,
    workExperienceInformation :: [WorkExperienceInformationItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PersonalInfo = PersonalInfo
  { displayName :: String,
    jobTitle :: String,
    addressLines :: [String],
    contactInformation :: PersonalInfoContactInfo,
    shortIntro :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PersonalInfoContactInfo = PersonalInfoContactInfo
  { phoneNumbers :: [String],
    emails :: [String],
    websites :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AppearancePreferences = AppearancePreferences
  { theme :: String,
    themeSections :: AnachronicThemeSections
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data WorkExperienceInformationItem = WorkExperienceInformationItem
  { employerName :: String,
    experiencePoints :: [String],
    positionName :: String,
    timeWorked :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AnachronicThemeSections = AnachronicThemeSections
  { shortIntroSectionTitle :: String,
    workExperienceSectionTitle :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

getConfig :: IO (Maybe Preferences)
getConfig = do
  Data.Yaml.decodeFileThrow ".hsresumebuilder.yaml" :: IO (Maybe Preferences)
