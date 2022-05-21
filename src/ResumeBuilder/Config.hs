module ResumeBuilder.Config where

import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import ResumeBuilder.ResumeBuilderModel (HsResumeBuilder, Preferences)

getConfig :: IO (Maybe HsResumeBuilder)
getConfig = do
  Data.Yaml.decodeFileThrow "hsresumebuilder.yaml" :: IO (Maybe HsResumeBuilder)