module ResumeBuilder.Config where

import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import ResumeBuilder.ResumeBuilderPreferences (Preferences)

getConfig :: IO (Maybe Preferences)
getConfig = do
  Data.Yaml.decodeFileThrow ".hsresumebuilder.yaml" :: IO (Maybe Preferences)
