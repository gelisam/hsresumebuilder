{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ResumeBuilder.ResumeBuilderModel where

import qualified Data.ByteString
import Data.Map (Map)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)

newtype HsResumeBuilder = HsResumeBuilder
  { hsResumeBuilder :: HsResumeBuilderPreferences
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype HsResumeBuilderPreferences = HsResumeBuilderPreferences
  { preferences :: Preferences
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Preferences = Preferences
  { personal :: PersonalInfo,
    appearance :: AppearancePreferences,
    experience :: [ExperienceItem],
    education :: [ExperienceItem],
    interestsHobbies :: [String],
    driverLicense :: [String],
    languages :: Languages,
    publications :: [ExperienceItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Languages = Languages
  { complexModeContent :: [LanguageLevel],
    simpleMode :: Bool,
    simpleModeContent :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data LanguageLevel = LanguageLevel
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
    locations :: [String],
    contact :: PersonalInfoContactInfo,
    shortIntro :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PersonalInfoContactInfo = PersonalInfoContactInfo
  { phoneNumbers :: [String],
    emails :: [String],
    websites :: PersonalInfoWebsites
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PersonalInfoWebsites = PersonalInfoWebsites
  { blogs :: [String],
    github :: [String],
    linkedIn :: [String],
    twitter :: [String],
    youtube :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AppearancePreferences = AppearancePreferences
  { theme :: String,
    documentTitles :: DocumentTitles,
    themeSettings :: JoeThemeSettings
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data JoeThemeSettings = JoeThemeSettings
  { bodyColor :: String,
    jobTitleColor :: String,
    nameColor :: String,
    sectionTitlesColor :: String,
    sectionTitleBorderEnabled :: Bool,
    entityNameColor :: String,
    positionNameColor :: String,
    timeWorkedColor :: String,
    linkColor :: String,
    bodyFontFamily :: String,
    titleFontFamily :: String,
    fontSize1 :: String,
    fontSize2 :: String,
    fontSize3 :: String,
    customStylesheetsToLoad :: [String]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data ExperienceItem = ExperienceItem
  { entityName :: String,
    experiencePoints :: [String],
    positionName :: String,
    timeWorked :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DocumentTitles = DocumentTitles
  { shortIntroTitle :: String,
    workExperienceTitle :: String,
    educationTitle :: String,
    interestsHobbiesTitle :: String,
    driverLicenseTitle :: String,
    languagesTitle :: String,
    publicationsTitle :: String,
    seeMyWebsitesTitle :: String
  }
  deriving (Generic, Show, ToJSON, FromJSON)