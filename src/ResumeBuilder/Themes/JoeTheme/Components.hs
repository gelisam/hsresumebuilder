{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Components where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import ResumeBuilder.ResumeBuilderModel
import ResumeBuilder.Themes.JoeTheme.Styler (Classes, Styles, applyClasses, applyStyles)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

jHeader1 :: ToMarkup a => String -> String -> String -> a -> Html
jHeader1 color fontFamily fontSize = (h1 ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("font-family", fontFamily),
        ("font-size", fontSize)
      ]

jHeader2 :: ToMarkup a => String -> String -> String -> a -> Html
jHeader2 color fontFamily fontSize = (h2 ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("font-family", fontFamily),
        ("font-size", fontSize)
      ]

jShortText :: ToMarkup a => String -> String -> a -> Html
jShortText color fontSize = (H.div ! applyStyles css) . toHtml
  where
    css = [("color", color), ("font-size", fontSize)]

jText :: ToMarkup a => String -> String -> a -> Html
jText color fontSize = (p ! applyStyles css) . toHtml
  where
    css = [("color", color), ("font-size", fontSize)]

jIcon :: String -> Classes -> Html
jIcon color classes = (i ! applyClasses classes ! applyStyles [("color", color)]) ""

jSmall :: ToMarkup a => String -> a -> Html
jSmall color = (H.small ! applyStyles css) . toHtml
  where
    css = [("color", color)]

jJustified :: ToMarkup a => String -> String -> a -> Html
jJustified color fontSize = (p ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("text-align", "justify"),
        ("font-size", fontSize)
      ]

jLink :: String -> String -> String -> String -> Html
jLink color fontSize url = (a ! applyStyles css ! (A.href . fromString) url) . toHtml
  where
    css =
      [ ("color", color),
        ("padding", "1em"),
        ("font-size", fontSize)
      ]

-- For a prettier output when printing to pdf, prevent the body from being
-- broken in two by a page break.
--
-- Debugging tip: add "border: 1px solid black;" to visualize the sections and
-- make sure they are indeed small enough that you wouldn't mind wasting that
-- amount of space in order to bump it to the next page.
jShortSection :: Html -> Html
jShortSection = H.div ! A.style "break-inside: avoid;"

-- Similar to @map jShortSection@, but also prevents a page break between the
-- header and the first item.
jHeaderAndShortSections :: Html -> [Html] -> Html
jHeaderAndShortSections header (firstItem:items) = do
  jShortSection $ do
    header
    firstItem
  forM_ items $ \item -> do
    jShortSection $ do
      item
jTitleAndShortSections header items = do
  jShortSection $ do
    header
    sequence_ items


jListItem :: ToMarkup a => String -> String -> a -> Html
jListItem color fontSize = (li ! applyStyles css) . toHtml
  where
    css = [("color", color)]

jLanguageSection :: Languages -> Html
jLanguageSection languages =
  if simpleMode languages
    then p . toHtml $ simpleModeContent languages
    else H.div $ jLanguageTable languages

jLanguageTable :: Languages -> Html
jLanguageTable languages = table $ do
  tr $ do
    th ""
    th "SPEAKING"
    th "WRITING"
    th "READING"
  forM_
    (complexModeContent languages)
    (tr . jLanguageLevelRow)

jExperienceItem :: JoeThemeSettings -> ExperienceItem -> Html
jExperienceItem themeSettings item = H.div $ do
  let bodyColor' = bodyColor themeSettings
  let bodyFontSize = fontSize3 themeSettings
  let entityNameColor' = entityNameColor themeSettings
  let positionNameColor' = positionNameColor themeSettings
  let timeWorkedColor' = timeWorkedColor themeSettings

  H.div ! A.style "display: flex;" $ do
    (strong ! applyStyles [("color", positionNameColor')]) . toHtml . positionName $ item
    ( H.span
        ! applyStyles
          [ ("margin-left", "0.4em"),
            ("margin-right", "0.4em"),
            ("color", bodyColor')
          ]
      )
      " - "
    (strong ! applyStyles [("color", entityNameColor')]) . toHtml . entityName $ item
  jSmall timeWorkedColor' $ timeWorked item
  ul $ forM_ (experiencePoints item) (jListItem bodyColor' bodyFontSize)

jLanguageLevelRow :: LanguageLevel -> Html
jLanguageLevelRow item = do
  td . strong . toHtml . languageName $ item
  td . toHtml . showLanguageLevel . speakingProficiency $ item
  td . toHtml . showLanguageLevel . writingProficiency $ item
  td . toHtml . showLanguageLevel . readingProficiency $ item

showLanguageLevel :: Int -> String
showLanguageLevel rating =
  case rating of
    0 -> "limited proficiency"
    1 -> "limited proficiency +"
    2 -> "high proficiency"
    3 -> "almost native"
    4 -> "native"
    _ -> ""

jFlexContainer :: Html -> Html
jFlexContainer =
  H.div
    ! applyStyles
      [ ("display", "flex"),
        ("align-items", "center"),
        ("gap", "0.5em")
      ]

jFlexContainer' :: Html -> Html
jFlexContainer' = do
  H.div
    ! applyStyles
      [ ("display", "flex"),
        ("align-items", "center"),
        ("gap", "1em")
      ]

jIconWithText :: ToMarkup a => String -> String -> String -> Classes -> a -> Html
jIconWithText bodyColor iconColor fontSize classes item = do
  jFlexContainer $ do
    jIcon iconColor classes
    jShortText bodyColor fontSize item

loadStylesheet :: String -> Html
loadStylesheet x = link ! rel "stylesheet" ! href (fromString x)

internalThemeStylesheets :: [String]
internalThemeStylesheets =
  [ "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css",
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css",
    "https://fonts.googleapis.com",
    "https://fonts.gstatic.com"
  ]

jSectionHeader :: ToMarkup a => String -> String -> String -> Bool -> a -> Html
jSectionHeader color fontFamily fontSize enableBorder =
  do
    let css =
          [ ("color", color),
            ("font-family", fontFamily),
            ("font-size", fontSize),
            if enableBorder
              then ("border-bottom", "0.8px solid" ++ color)
              else ("border-bottom", "none")
          ]
    (h3 ! applyStyles css) . toHtml