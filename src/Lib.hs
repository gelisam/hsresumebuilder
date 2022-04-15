{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Config
import Control.Monad (forM_)
import System.Exit (exitFailure)
import System.IO
  ( IOMode (ReadWriteMode),
    hClose,
    hGetContents,
    openFile,
  )
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

someFunc :: IO ()
someFunc = do
  maybeParsedConfig <- getConfig
  maybe exitFailure beginRendering maybeParsedConfig

beginRendering :: Preferences -> IO ()
beginRendering config = do
  let generatedContent = renderHtml . anachronicTheme $ config
  saveContentToFile generatedContent
  putStrLn generatedContent

saveContentToFile :: String -> IO ()
saveContentToFile content = do
  handle <- openFile "output.html" ReadWriteMode
  contents <- hGetContents handle
  hClose handle
  writeFile "output.html" content

anachronicTheme :: Preferences -> Html
anachronicTheme config = docTypeHtml $ do
  H.head $ do
    H.title . toHtml $ ("Resume of " ++ (displayName . personalInformation $ config))
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css"
  body $ do
    main $ do
      h1 . toHtml . displayName . personalInformation $ config
      (h2 ! A.style "color: darkgreen;") . toHtml . jobTitle . personalInformation $ config
      H.div ! A.style "display: flex" $ do
        H.div ! A.style "padding: 1em;" $ do
          forM_ (addressLines . personalInformation $ config) (p . toHtml)
        H.div ! A.style "padding: 1em;" $ do
          forM_ (phoneNumbers . contactInformation . personalInformation $ config) (p . toHtml)
          forM_ (emails . contactInformation . personalInformation $ config) (p . toHtml)

      -- shortIntroSection
      H.div $ do
        h3 . toHtml $ (shortIntroSectionTitle . documentTitles . appearancePreferences $ config)
        forM_ (shortIntro . personalInformation $ config) ((p ! A.style "text-align: justify") . toHtml)

      -- workExperienceSection
      H.div $ do
        h3 . toHtml $ (workExperienceSectionTitle . documentTitles . appearancePreferences $ config)
        forM_ (workExperienceInformation config) renderWorkExperienceItemComponent

      -- educationSection
      H.div $ do
        h3 . toHtml $ (educationSectionTitle . documentTitles . appearancePreferences $ config)
        forM_ (educationInformation config) renderWorkExperienceItemComponent

      -- driverLicenseSection
      H.div $ do
        h3 . toHtml $ (driverLicenseInformationTitle . documentTitles . appearancePreferences $ config)
        ul $ forM_ (driverLicenseInformation config) (li . toHtml)

      -- languagesSection
      H.div $ do
        h3 . toHtml $ (languagesInformationTitle . documentTitles . appearancePreferences $ config)

      -- interestsHobbiesSection
      H.div $ do
        h3 . toHtml $ (interestsHobbiesInformationTitle . documentTitles . appearancePreferences $ config)
        forM_ (interestsHobbiesInformation config) (p . toHtml)
        h5 . toHtml $ (seeMyWebsitesSectionTitle . documentTitles . appearancePreferences $ config)
        forM_ (websites . contactInformation . personalInformation $ config) (p . toHtml)

-- (p ! A.style "text-align: justify") . toHtml $ (shortIntroSectionBody . documentTitles . appearancePreferences $ config)
renderWorkExperienceItemComponent :: WorkExperienceInformationItem -> Html
renderWorkExperienceItemComponent item = do
  H.div $ do
    strong . toHtml . positionName $ item
    (strong ! A.style "color: grey") . toHtml . entityName $ item
    p . toHtml . timeWorked $ item
    ul $ forM_ (experiencePoints item) (li . toHtml)