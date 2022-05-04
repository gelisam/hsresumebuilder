{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.JoeTheme where

import Control.Monad (forM_)
import Data.String (fromString)
import ResumeBuilder.Config
import System.Exit (exitFailure)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

attributeCSSColor :: String -> String
attributeCSSColor colorValue = "color: " ++ colorValue ++ ";"

attributeBodyFontFamily :: String -> String
attributeBodyFontFamily font = "font-family: " ++ font ++ ";"

attributeTitleFontFamily :: String -> String
attributeTitleFontFamily font = "font-family: " ++ font ++ ";"

componentSectHeader :: ToMarkup a => String -> a -> Html
componentSectHeader color = (h3 ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentPersonNameHeader :: ToMarkup a => String -> a -> Html
componentPersonNameHeader color = (h1 ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentJobTitleHeader :: ToMarkup a => String -> a -> Html
componentJobTitleHeader color = (h2 ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentBodyText :: ToMarkup a => String -> a -> Html
componentBodyText color = (p ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentSmallBodyText :: ToMarkup a => String -> a -> Html
componentSmallBodyText color = (small ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentJustifiedBodyText :: ToMarkup a => String -> a -> Html
componentJustifiedBodyText color = (p ! A.style (fromString $ "text-align: justify;" ++ attributeCSSColor color)) . toHtml

componentBodyLink :: ToMarkup a => String -> a -> Html
componentBodyLink color = do
  let linkStyle = attributeCSSColor color ++ "padding: 1em;"
  (a ! A.style (fromString linkStyle) ! A.href "") . toHtml

componentBodyListItem :: ToMarkup a => String -> a -> Html
componentBodyListItem color = (li ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentWorkExperienceItem :: JoeThemeSettings -> WorkExperienceInformationItem -> Html
componentWorkExperienceItem themeSettings item = H.div $ do
  let bodyColor' = bodyColor themeSettings
  let entityNameColor' = workExperienceInformationEntityNameColor themeSettings
  let positionNameColor' = workExperienceInformationPositionNameColor themeSettings
  let timeWorkedColor' = workExperienceInformationPositionNameColor themeSettings

  H.div ! A.style "display: flex;" $ do
    (strong ! A.style (fromString $ attributeCSSColor positionNameColor')) . toHtml . positionName $ item
    (H.span ! (A.style . fromString $ "margin-left: 0.4em; margin-right: 0.4em; color: " ++ bodyColor' ++ ";")) " - "
    (strong ! A.style (fromString (attributeCSSColor entityNameColor'))) . toHtml . entityName $ item
  componentSmallBodyText timeWorkedColor' $ timeWorked item
  ul $ forM_ (experiencePoints item) (componentBodyListItem bodyColor')

componentLanguageLevelItem :: LanguageLevelInformation -> Html
componentLanguageLevelItem item = do
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

joeTheme :: Preferences -> Html
joeTheme config = docTypeHtml $ do
  let documentTitles' = documentTitles . appearancePreferences $ config
  let themeSettings' = themeSettings . appearancePreferences $ config
  let personalInformation' = personalInformation config
  let bodyColor' = bodyColor themeSettings'
  let linkColor' = linkColor themeSettings'
  let sectionTitlesColor' = sectionTitlesColor themeSettings'
  let bodyFontFamily' = bodyFontFamily themeSettings'

  H.head $ do
    H.title . toHtml $ "Resume of " ++ (displayName . personalInformation $ config)
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css"
    link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css"
  body $
    main ! (A.style . fromString . attributeBodyFontFamily $ bodyFontFamily') $ do
      componentPersonNameHeader
        (nameColor themeSettings')
        $ displayName personalInformation'

      componentJobTitleHeader
        (jobTitleColor themeSettings')
        $ jobTitle personalInformation'

      -- Contact details section
      H.div ! A.style "display: flex" $ do
        H.div ! A.style "padding: 1em;" $ forM_ (addressLines personalInformation') (componentBodyText bodyColor')
        H.div ! A.style "padding: 1em;" $ do
          forM_ (phoneNumbers . contactInformation $ personalInformation') $ do
            componentBodyText bodyColor'

          forM_ (emails . contactInformation $ personalInformation') $ do
            componentBodyText bodyColor'
      -- H.i ! (A.class_ . fromString "fa-solid fa-envelope")

      -- Short introduction section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ shortIntroSectionTitle documentTitles'
        forM_ (shortIntro personalInformation') (componentJustifiedBodyText bodyColor')

      -- Work experience section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ workExperienceSectionTitle documentTitles'
        forM_ (workExperienceInformation config) (componentWorkExperienceItem themeSettings')

      br

      -- Education section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ educationSectionTitle documentTitles'
        forM_ (educationInformation config) (componentWorkExperienceItem themeSettings')

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
                (tr . componentLanguageLevelItem)

      -- Driver license section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ driverLicenseInformationTitle documentTitles'
        ul $ forM_ (driverLicenseInformation config) (componentBodyListItem bodyColor')

      -- Interests hobbies section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ interestsHobbiesInformationTitle documentTitles'
        forM_ (interestsHobbiesInformation config) (componentJustifiedBodyText bodyColor')

      -- Websites section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          $ seeMyWebsitesSectionTitle documentTitles'
        forM_ (websites . contactInformation $ personalInformation') (componentBodyLink linkColor')
