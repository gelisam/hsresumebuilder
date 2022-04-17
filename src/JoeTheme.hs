{-# LANGUAGE OverloadedStrings #-}

module JoeTheme where

import Config
import Control.Monad (forM_)
import Data.String
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

attributeCSSColor :: String -> String
attributeCSSColor colorValue = "color: " ++ colorValue ++ ";"

componentSectHeader :: ToMarkup a => String -> a -> Html
componentSectHeader color = (h3 ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentPersonNameHeader :: ToMarkup a => Preferences -> a -> Html
componentPersonNameHeader config = (h1 ! A.style (fromString $ attributeCSSColor (sectionTitlesColor . themeSettings . appearancePreferences $ config))) . toHtml

componentJobTitleHeader :: ToMarkup a => Preferences -> a -> Html
componentJobTitleHeader config = (h2 ! A.style (fromString $ attributeCSSColor (sectionTitlesColor . themeSettings . appearancePreferences $ config))) . toHtml

componentBodyText :: ToMarkup a => Preferences -> a -> Html
componentBodyText config = (p ! A.style (fromString $ attributeCSSColor (bodyColor . themeSettings . appearancePreferences $ config))) . toHtml

componentSmallBodyText :: ToMarkup a => Preferences -> a -> Html
componentSmallBodyText config = (small ! A.style (fromString $ attributeCSSColor (bodyColor . themeSettings . appearancePreferences $ config))) . toHtml

componentJustifiedBodyText :: ToMarkup a => Preferences -> a -> Html
componentJustifiedBodyText config = (p ! A.style (fromString $ "text-align: justify;" ++ attributeCSSColor (bodyColor . themeSettings . appearancePreferences $ config))) . toHtml

componentBodyLink :: ToMarkup a => Preferences -> a -> Html
componentBodyLink config = do
  let linkStyle = attributeCSSColor (linkColor . themeSettings . appearancePreferences $ config) ++ "padding: 1em;"
  (a ! A.style (fromString linkStyle) ! A.href "") . toHtml

componentBodyListItem :: ToMarkup a => Preferences -> a -> Html
componentBodyListItem config = (li ! A.style (fromString $ attributeCSSColor (bodyColor . themeSettings . appearancePreferences $ config))) . toHtml

getBodyFontFamily :: Preferences -> String
getBodyFontFamily config = "font-family: " ++ (bodyFontFamily . themeSettings . appearancePreferences $ config) ++ ";"

getTitleFontFamily :: Preferences -> String
getTitleFontFamily config = "font-family: " ++ (bodyFontFamily . themeSettings . appearancePreferences $ config) ++ ";"

renderWorkExperienceItemComponent :: Preferences -> WorkExperienceInformationItem -> Html
renderWorkExperienceItemComponent config item = H.div $ do
  H.div ! A.style "display: flex;" $ do
    (strong ! A.style (fromString $ attributeCSSColor . workExperienceInformationPositionNameColor . themeSettings . appearancePreferences $ config)) . toHtml . positionName $ item
    (H.span ! (A.style . fromString $ "margin-left: 0.4em; margin-right: 0.4em; color: " ++ (bodyColor . themeSettings . appearancePreferences $ config) ++ ";")) " - "
    (strong ! A.style (fromString (attributeCSSColor . workExperienceInformationEntityNameColor . themeSettings . appearancePreferences $ config))) . toHtml . entityName $ item
  componentSmallBodyText config $ timeWorked item
  ul $ forM_ (experiencePoints item) (componentBodyListItem config)

renderLanguageLevelInformationComponent :: LanguageLevelInformation -> Html
renderLanguageLevelInformationComponent item = do
  td . strong . toHtml . languageName $ item
  td . toHtml . languageLevelRatingString . speakingProficiency $ item
  td . toHtml . languageLevelRatingString . writingProficiency $ item
  td . toHtml . languageLevelRatingString . readingProficiency $ item

languageLevelRatingString :: Int -> String
languageLevelRatingString rating =
  case rating of
    0 -> "limited proficiency"
    1 -> "limited proficiency +"
    2 -> "high proficiency"
    3 -> "almost native"
    4 -> "native"
    _ -> ""

sectionContactDetails :: Preferences -> Html
sectionContactDetails config = do
  H.div ! A.style "display: flex" $ do
    H.div ! A.style "padding: 1em;" $ forM_ (addressLines . personalInformation $ config) (componentBodyText config)
    H.div ! A.style "padding: 1em;" $ do
      forM_ (phoneNumbers . contactInformation . personalInformation $ config) (componentBodyText config)
      forM_ (emails . contactInformation . personalInformation $ config) (componentBodyText config)

joeTheme :: Preferences -> Html
joeTheme config = docTypeHtml $ do
  let sectionTitlesColor' = sectionTitlesColor . themeSettings . appearancePreferences $ config
  let documentTitles' = documentTitles . appearancePreferences $ config
  let jobTitleColor' = jobTitleColor . themeSettings . appearancePreferences $ config
  H.head $ do
    H.title . toHtml $ "Resume of " ++ (displayName . personalInformation $ config)
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css"
  body $
    main ! (A.style . fromString . getBodyFontFamily $ config) $ do
      componentPersonNameHeader config $ displayName . personalInformation $ config

      componentJobTitleHeader config $ jobTitle . personalInformation $ config

      -- Contact details section
      sectionContactDetails config

      -- Short introduction section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ shortIntroSectionTitle documentTitles'
        forM_ (shortIntro . personalInformation $ config) (componentJustifiedBodyText config)

      -- Work experience section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ workExperienceSectionTitle documentTitles'
        forM_ (workExperienceInformation config) (renderWorkExperienceItemComponent config)

      br

      -- Education section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ educationSectionTitle documentTitles'
        forM_ (educationInformation config) (renderWorkExperienceItemComponent config)

      -- Languages section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ languagesInformationTitle documentTitles'
        if simpleMode . languagesInformation $ config
          then p . toHtml $ simpleModeContent . languagesInformation $ config
          else H.div $ do
            table $ do
              tr $ do
                th ""
                th "SPEAKING"
                th "WRITING"
                th "READING"
              forM_
                (complexModeContent . languagesInformation $ config)
                (tr . renderLanguageLevelInformationComponent)

      -- Driver license section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ driverLicenseInformationTitle documentTitles'
        ul $ forM_ (driverLicenseInformation config) (componentBodyListItem config)

      -- Interests hobbies section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ interestsHobbiesInformationTitle documentTitles'
        forM_ (interestsHobbiesInformation config) (componentJustifiedBodyText config)

      -- Websites section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ seeMyWebsitesSectionTitle documentTitles'
        forM_ (websites . contactInformation . personalInformation $ config) (componentBodyLink config)
