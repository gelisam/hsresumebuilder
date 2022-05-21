{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Template where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import ResumeBuilder.ResumeBuilderPreferences
import ResumeBuilder.Themes.JoeTheme.Components
import ResumeBuilder.Themes.JoeTheme.Styler (applyStyles)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderResume :: Preferences -> Html
renderResume config = docTypeHtml $ do
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
    -- meta
    meta ! charset "UTF-8"

  body $
    main ! applyStyles [("font-family", bodyFontFamily')] $ do
      H.div
        ! applyStyles
          [ ("display", "flex"),
            ("flex-direction", "row")
          ]
        $ do
          H.div ! applyStyles [("flex-grow", "1")] $ do
            componentPersonNameHeader
              (nameColor themeSettings')
              (titleFontFamily themeSettings')
              $ displayName personalInformation'

            componentJobTitleHeader
              (jobTitleColor themeSettings')
              (titleFontFamily themeSettings')
              $ jobTitle personalInformation'

          -- Contact details section"
          H.div ! applyStyles [("flex-grow", "0.6"), ("display", "flex")] $ do
            H.div ! applyStyles [("padding", "0.8em")] $ forM_ (addressLines personalInformation') (componentBodyText bodyColor')
            H.div ! applyStyles [("padding", "0.8em")] $ do
              forM_ (phoneNumbers . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' ["fa-solid", "fa-phone"])
              forM_ (emails . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' ["fa-solid", "fa-envelope"])

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
          forM_ (blogs . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' ["fa-solid", "fa-rss"])
          forM_ (github . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' ["fa-brands", "fa-github"])
          forM_ (linkedIn . websites . contactInformation $ personalInformation') (componentIconPrecedingText bodyColor' ["fa-brands", "fa-linkedin"])

      H.div ! applyStyles [("margin-top", "16px"), ("text-align", "center")] $ do
        small $ do
          (a ! href "https://github.com/averageflow/hsresumebuilder")
            "λ This document has been proudly generated with Haskell code"
