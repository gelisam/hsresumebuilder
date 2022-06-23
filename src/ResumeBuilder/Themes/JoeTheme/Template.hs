{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Template where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import ResumeBuilder.ResumeBuilderModel
import ResumeBuilder.Themes.JoeTheme.Components
import ResumeBuilder.Themes.JoeTheme.Styler (applyStyles)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderResume :: Preferences -> Html
renderResume config = docTypeHtml $ do
  let documentTitles' = documentTitles . appearance $ config
      theme' = themeSettings . appearance $ config
      personal' = personal config
      bodyColor' = bodyColor theme'
      linkColor' = linkColor theme'
      sectionHeaderColor = sectionTitlesColor theme'
      bodyFontFamily' = bodyFontFamily theme'
      fontSize3' = fontSize3 theme'
      renderSectionHeader headerText =
        jSectionHeader
          sectionHeaderColor
          (titleFontFamily theme')
          (fontSize2 theme')
          (sectionTitleBorderEnabled theme')
          headerText
      renderShortSection headerText body = do
        jShortSection $ do
          renderSectionHeader headerText
          body
      renderLongSection headerText = do
        jHeaderAndShortSections (renderSectionHeader headerText)

      renderPersonNameHeader headerText =
        jHeader1
          (nameColor theme')
          (titleFontFamily theme')
          (fontSize1 theme')
          headerText

      renderJobTitleHeader headerText =
        jHeader2
          (jobTitleColor theme')
          (titleFontFamily theme')
          (fontSize2 theme')
          headerText

  H.head $ do
    H.title . toHtml $ "Resume of " ++ (displayName . personal $ config)
    -- load internal theme stylesheets
    forM_ internalThemeStylesheets loadStylesheet
    -- load custom user stylesheets
    forM_ (customStylesheetsToLoad theme') loadStylesheet
    -- meta
    meta ! charset "UTF-8"

  H.body $
    main ! applyStyles [("font-family", bodyFontFamily')] $ do
      H.div
        ! applyStyles
          [ ("display", "flex"),
            ("flex-direction", "row")
          ]
        $ do
          -- Name and job title section
          H.div ! applyStyles [("flex-grow", "1")] $ do
            renderPersonNameHeader . displayName $ personal'
            renderJobTitleHeader . jobTitle $ personal'

          -- Contact details section
          H.div ! applyStyles [("flex-grow", "0.6"), ("display", "flex")] $ do
            H.div ! applyStyles [("padding", "0.8em")] $ do
              forM_
                (addressLines personal')
                (jText bodyColor' (fontSize3 theme'))
            H.div ! applyStyles [("padding", "0.8em")] $ do
              forM_
                (emails . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-envelope"]
                )
              forM_
                (phoneNumbers . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-phone"]
                )
              forM_
                (locations personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-location-dot"]
                )
            H.div ! applyStyles [("padding", "0.8em")] $ do
              forM_
                (github . websites . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    (fontSize3 theme')
                    ["fa-brands", "fa-github"]
                )
              forM_
                (youtube . websites . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    (fontSize3 theme')
                    ["fa-brands", "fa-youtube"]
                )
              forM_
                (blogs . websites . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    (fontSize3 theme')
                    ["fa-solid", "fa-pen"]
                )
              forM_
                (linkedIn . websites . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    (fontSize3 theme')
                    ["fa-brands", "fa-linkedin"]
                )
              forM_
                (twitter . websites . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    (fontSize3 theme')
                    ["fa-brands", "fa-twitter"]
                )

      -- Short introduction section
      renderShortSection (shortIntroTitle documentTitles') $ do
        forM_ (shortIntro personal') (jJustified bodyColor' (fontSize3 theme'))

      -- Work experience section
      renderLongSection (workExperienceTitle documentTitles')
        (fmap (jExperienceItem theme') (experience config))

      -- Education section
      renderLongSection (educationTitle documentTitles')
        (fmap (jExperienceItem theme') (education config))

      -- Credits to hsResumeBuilder
      jShortSection $ do
        H.div ! applyStyles [("margin-top", "16px"), ("text-align", "center")] $ do
          small $ do
            jLink
              linkColor'
              fontSize3'
              "https://github.com/gelisam/hsresumebuilder"
              "λ This document has been proudly auto-generated with Haskell code"
