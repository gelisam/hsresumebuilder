{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Components where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import ResumeBuilder.ResumeBuilderPreferences
import ResumeBuilder.Themes.JoeTheme.Styler (Classes, Styles, applyClasses, applyStyles)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

componentPersonNameHeader :: ToMarkup a => String -> String -> a -> Html
componentPersonNameHeader color fontFamily = (h1 ! applyStyles css) . toHtml
  where
    css = [("color", color), ("font-family", fontFamily)]

componentJobTitleHeader :: ToMarkup a => String -> String -> a -> Html
componentJobTitleHeader color fontFamily = (h2 ! applyStyles css) . toHtml
  where
    css = [("color", color), ("font-family", fontFamily)]

componentBodyText :: ToMarkup a => String -> a -> Html
componentBodyText color = (p ! applyStyles css) . toHtml
  where
    css = [("color", color)]

componentStandaloneIcon :: Classes -> Html
componentStandaloneIcon classes = (i ! applyClasses classes) ""

componentSmallBodyText :: ToMarkup a => String -> a -> Html
componentSmallBodyText color = (small ! applyStyles css) . toHtml
  where
    css = [("color", color)]

componentJustifiedBodyText :: ToMarkup a => String -> a -> Html
componentJustifiedBodyText color = (p ! applyStyles css) . toHtml
  where
    css = [("color", color), ("text-align", "justify")]

componentBodyLink :: ToMarkup a => String -> a -> Html
componentBodyLink color = (a ! applyStyles css ! A.href "") . toHtml
  where
    css = [("color", color), ("padding", "1em")]

componentBodyListItem :: ToMarkup a => String -> a -> Html
componentBodyListItem color = (li ! applyStyles css) . toHtml
  where
    css = [("color", color)]

componentWorkExperienceItem :: JoeThemeSettings -> WorkExperienceInformationItem -> Html
componentWorkExperienceItem themeSettings item = H.div $ do
  let bodyColor' = bodyColor themeSettings
  let entityNameColor' = workExperienceInformationEntityNameColor themeSettings
  let positionNameColor' = workExperienceInformationPositionNameColor themeSettings
  let timeWorkedColor' = workExperienceInformationPositionNameColor themeSettings

  H.div ! A.style "display: flex;" $ do
    (strong ! applyStyles [("color", positionNameColor')]) . toHtml . positionName $ item
    (H.span ! applyStyles [("margin-left", "0.4em"), ("margin-right", "0.4em"), ("color", bodyColor')]) " - "
    (strong ! applyStyles [("color", entityNameColor')]) . toHtml . entityName $ item
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
componentIconPrecedingTextContainer = do
  H.div ! applyStyles [("display", "flex"), ("align-items", "center"), ("gap", "0.5em")]

componentIconPrecedingTextContainer' :: Html -> Html
componentIconPrecedingTextContainer' = do
  H.div ! applyStyles [("display", "flex"), ("align-items", "center"), ("gap", "1em")]

componentIconPrecedingText :: ToMarkup a => String -> Classes -> a -> Html
componentIconPrecedingText bodyColor classes item = componentIconPrecedingTextContainer $ do
  componentStandaloneIcon classes
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

componentSectHeader :: ToMarkup a => String -> String -> a -> Html
componentSectHeader color fontFamily = do
  let css = [("color", color), ("font-family", fontFamily), ("border-bottom", "0.8px solid" ++ color)]
  (h3 ! applyStyles css) . toHtml