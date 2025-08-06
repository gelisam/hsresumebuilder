{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Lib
  ( resumeBuilder,
  )
where

import Control.Monad (forM_)
import ResumeBuilder.Config (getConfig)
import ResumeBuilder.ResumeBuilderModel (HsResumeBuilder (hsResumeBuilder), HsResumeBuilderPreferences (preferences), Preferences)
import ResumeBuilder.Themes.JoeTheme.Template (renderResume)
import System.Exit (exitFailure)
import System.IO
  ( IOMode (ReadWriteMode),
    hClose,
    hGetContents,
    openFile,
  )
import Text.Blaze.Html.Renderer.String (renderHtml)

resumeBuilder :: IO ()
resumeBuilder = do
  maybeParsedConfig <- getConfig
  maybe exitFailure beginRendering maybeParsedConfig

beginRendering :: HsResumeBuilder -> IO ()
beginRendering config = do
  let generatedContent = renderHtml . renderResume . preferences . hsResumeBuilder $ config
  saveContentToFile generatedContent
  putStrLn "SUCCESSFULLY GENERATED RESUME! SEE THE FILE NAMED docs/index.html IN THIS CURRENT DIRECTORY!"

saveContentToFile :: String -> IO ()
saveContentToFile content = do
  writeFile "docs/index.html" content
