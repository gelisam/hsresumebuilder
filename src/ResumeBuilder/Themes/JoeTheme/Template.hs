{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Template where

import Control.Monad (forM_)
import Data.List (intersperse)
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

      renderJobTitleHeader jobTitles = do
        jHeader2
            (jobTitleColor theme')
            (titleFontFamily theme')
            (fontSize2 theme')
            $ do
          sequence_ $ intersperse br (fmap fromString jobTitles)

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
          H.div ! applyStyles [("flex-grow", "0")] $ do
            renderPersonNameHeader . displayName $ personal'
            renderJobTitleHeader . jobTitles $ personal'

          -- Gap
          H.div ! applyStyles [("flex-grow", "1"), ("display", "flex")] $ do
            pure ()

          -- Contact details section
          H.div ! applyStyles [("flex-grow", "0"), ("display", "flex")] $ do
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
              forM_
                (languages personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-globe"]
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

      -- AI Safety Exposure section
      shortLineHeight $
        renderLongSection (aiSafetyExposureTitle documentTitles')
          (fmap (jSingleItem theme') (aiSafetyExposure config))

      -- Publications section
      renderLongSection (publicationsTitle documentTitles')
        (fmap (jParagraphGenericItem theme' " " "by ") (publications config))

      -- Work experience section
      renderLongSection (workExperienceTitle documentTitles')
        (fmap (jExperienceItem theme' " " "at ") (experience config))

      -- Education section
      renderLongSection (educationTitle documentTitles')
        (fmap (jParagraphGenericItem theme' " " "from ") (education config))

      -- Hobbies section
      shortLineHeight $
        renderLongSection (interestsHobbiesTitle documentTitles')
          (fmap (jSingleItem theme') (interestsHobbies config))

      -- Praise section
      renderLongSection (praiseTitle documentTitles')
        (fmap (jParagraphGenericItem theme' ", " "") (praise config))
