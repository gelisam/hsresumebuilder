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

attributeFontFamily :: String -> String
attributeFontFamily font = "font-family: " ++ font ++ ";"

componentSectHeader :: ToMarkup a => String -> String -> a -> Html
componentSectHeader color fontFamily item = do
  let componentStyle = fromString (attributeCSSColor color) ++ fromString (attributeFontFamily fontFamily) ++ fromString "border-bottom:0.8px solid " ++ color
  (h3 ! A.style (fromString componentStyle)) . toHtml $ item

componentPersonNameHeader :: ToMarkup a => String -> String -> a -> Html
componentPersonNameHeader color fontFamily item = do
  let componentStyle = fromString (attributeCSSColor color) ++ fromString (attributeFontFamily fontFamily)
  (h1 ! A.style (fromString componentStyle)) . toHtml $ item

componentJobTitleHeader :: ToMarkup a => String -> String -> a -> Html
componentJobTitleHeader color fontFamily item = do
  let componentStyle = fromString (attributeCSSColor color) ++ fromString (attributeFontFamily fontFamily)
  (h2 ! A.style (fromString componentStyle)) . toHtml $ item

componentBodyText :: ToMarkup a => String -> a -> Html
componentBodyText color = (p ! A.style (fromString $ attributeCSSColor color)) . toHtml

componentStandaloneIcon :: String -> Html
componentStandaloneIcon classValue = (i ! A.class_ (fromString classValue)) ""

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

componentIconPrecedingTextContainer :: Html -> Html
componentIconPrecedingTextContainer = H.div ! A.style "display: flex; align-items: center; gap: 0.5em;"

componentIconPrecedingTextContainer' :: Html -> Html
componentIconPrecedingTextContainer' = H.div ! A.style "display: flex; align-items: center; gap: 1em;"

componentIconPrecedingText :: ToMarkup a => String -> String -> a -> Html
componentIconPrecedingText bodyColor iconClass item = componentIconPrecedingTextContainer $ do
  componentStandaloneIcon iconClass
  componentBodyText bodyColor item

loadStylesheet :: String -> Html
loadStylesheet x = link ! rel "stylesheet" ! href (fromString x)

internalThemeStylesheets :: [String]
internalThemeStylesheets =
  [ "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css",
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css",
    "https://fonts.googleapis.com",
    "https://fonts.gstatic.com"
  ]

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
    -- load internal theme stylesheets
    forM_ internalThemeStylesheets loadStylesheet
    -- load custom user stylesheets
    forM_ (customStylesheetsToLoad themeSettings') loadStylesheet
  body $
    main ! (A.style . fromString . attributeFontFamily $ bodyFontFamily') $ do
      H.div ! A.style "display:flex;flex-direction:row;" $ do
        H.div ! A.style "flex-grow: 1" $ do
          componentPersonNameHeader
            (nameColor themeSettings')
            (titleFontFamily themeSettings')
            $ displayName personalInformation'

          componentJobTitleHeader
            (jobTitleColor themeSettings')
            (titleFontFamily themeSettings')
            $ jobTitle personalInformation'

        -- Contact details section
        H.div ! A.style "flex-grow:0.6; display: flex" $ do
          H.div ! A.style "padding: 1em;" $ forM_ (addressLines personalInformation') (componentBodyText bodyColor')
          H.div ! A.style "padding: 1em;" $ do
            forM_ (phoneNumbers . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' "fa-solid fa-phone")
            forM_ (emails . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' "fa-solid fa-envelope")

      -- Short introduction section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ shortIntroSectionTitle documentTitles'
        forM_ (shortIntro personalInformation') (componentJustifiedBodyText bodyColor')

      -- Work experience section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ workExperienceSectionTitle documentTitles'
        forM_ (workExperienceInformation config) (componentWorkExperienceItem themeSettings')

      br

      -- Education section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ educationSectionTitle documentTitles'
        forM_ (educationInformation config) (componentWorkExperienceItem themeSettings')

      -- Languages section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ languagesInformationTitle documentTitles'
        if simpleMode . languagesInformation $ config
          then p . toHtml $ simpleModeContent . languagesInformation $ config
          else H.div $
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
          (titleFontFamily themeSettings')
          $ driverLicenseInformationTitle documentTitles'
        ul $ forM_ (driverLicenseInformation config) (componentBodyListItem bodyColor')

      -- Interests hobbies section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ interestsHobbiesInformationTitle documentTitles'
        forM_ (interestsHobbiesInformation config) (componentJustifiedBodyText bodyColor')

      -- Websites section
      H.div $ do
        componentSectHeader
          sectionTitlesColor'
          (titleFontFamily themeSettings')
          $ seeMyWebsitesSectionTitle documentTitles'
        componentIconPrecedingTextContainer' $ do
          forM_ (blogs . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' "fa-solid fa-rss")
          forM_ (github . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' "fa-brands fa-github")
          forM_ (linkedIn . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' "fa-brands fa-linkedin")

      H.div ! A.style "margin-top: 16px; text-align:center;" $ do
        small $ do
          (a ! href "https://github.com/averageflow/hsresumebuilder")
            "λ This document has been proudly generated with my own Haskell code"
