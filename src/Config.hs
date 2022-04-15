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
    workExperienceInformation :: [WorkExperienceInformationItem],
    educationInformation :: [WorkExperienceInformationItem],
    interestsHobbiesInformation :: [String],
    driverLicenseInformation :: [String],
    languagesInformation :: LanguagesInformation
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data LanguagesInformation = LanguagesInformation
  { complexModeContent :: [LanguageLevelInformation],
    simpleMode :: Bool,
    simpleModeContent :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data LanguageLevelInformation = LanguageLevelInformation
  { languageName :: String,
    speakingProficiency :: Int,
    writingProficiency :: Int,
    readingProficiency :: Int
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
    documentTitles :: DocumentTitles
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data WorkExperienceInformationItem = WorkExperienceInformationItem
  { entityName :: String,
    experiencePoints :: [String],
    positionName :: String,
    timeWorked :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DocumentTitles = DocumentTitles
  { shortIntroSectionTitle :: String,
    workExperienceSectionTitle :: String,
    educationSectionTitle :: String,
    interestsHobbiesInformationTitle :: String,
    driverLicenseInformationTitle :: String,
    languagesInformationTitle :: String,
    seeMyWebsitesSectionTitle :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

getConfig :: IO (Maybe Preferences)
getConfig = do
  Data.Yaml.decodeFileThrow ".hsresumebuilder.yaml" :: IO (Maybe Preferences)
