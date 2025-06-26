{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ResumeBuilder.Themes.JoeTheme.Components where

import Control.Monad (forM_, unless)
import Data.List (intersperse)
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
        ("font-size", fontSize),
        ("margin-top", "12px"),  -- override light.css's 24px
        ("margin-bottom", "3px")  -- override light.css's 12px
      ]

jHeader2 :: ToMarkup a => String -> String -> String -> a -> Html
jHeader2 color fontFamily fontSize = (h2 ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("font-family", fontFamily),
        ("font-size", fontSize),
        ("margin-top", "3px"),  -- override light.css's 24px
        ("margin-bottom", "3px")  -- override light.css's 12px
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
jIcon color classes = (i ! applyClasses classes ! applyStyles css) ""
  where
    css =
      [ ("color", color),
        ("width", "1.2em"),
        ("text-align", "center")
      ]

jSmall :: ToMarkup a => String -> a -> Html
jSmall color = (H.small ! applyStyles css) . toHtml
  where
    css = [("color", color)]

jParagraph :: ToMarkup a => String -> String -> a -> Html
jParagraph color fontSize = (p ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("text-align", "justify"),
        ("font-size", fontSize),
        ("margin-top", "0px"),
        ("margin-bottom", "0px")
      ]

jLink :: String -> String -> String -> Html -> Html
jLink color fontSize body = (a ! applyStyles css ! (A.href . fromString) body)
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
jShortSection = H.div ! applyStyles
  [ ("break-inside", "avoid")
  ]

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


jUnorderedList :: ToMarkup a => a -> Html
jUnorderedList = (ul ! applyStyles css) . toHtml
  where
    css =
      [ ("margin-top", "0px")
      ]

jListItem :: ToMarkup a => String -> String -> a -> Html
jListItem color fontSize = (li ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("text-align", "justify"),
        ("font-size", fontSize),
        ("margin-top", "0px")
      ]

jGenericItem
  :: JoeThemeSettings
  -> (a -> [String])
  -> (a -> String)
  -> (a -> String)
  -> Bool
  -> (a -> body)
  -> (body -> Html)
  -> String -> String -> a -> Html
jGenericItem themeSettings leftPieces middlePiece rightPiece addSpaceAbove details renderDetails separator1 separator2 item = H.div $ do
  let bodyColor' = bodyColor themeSettings
  let entityNameColor' = entityNameColor themeSettings
  let positionNameColor' = positionNameColor themeSettings
  let timeWorkedColor' = timeWorkedColor themeSettings

  unless (null . leftPieces $ item) $ do
    H.div ! applyStyles ( [ ("display", "flex")
                          , ("justify-content", "space-between")
                          ]
                       ++ if addSpaceAbove
                          then [("margin-top", "1em")]
                          else []
                        ) $ do
      H.span $ do
        (H.span ! applyStyles [("color", positionNameColor')]) $ do
          let stringPieces = leftPieces item
              htmlPieces = fmap toHtml stringPieces
              boldPieces = fmap H.strong htmlPieces
              commaSeparatedPieces = intersperse ", " boldPieces
          sequence_ commaSeparatedPieces
        (H.span ! applyStyles [("color", positionNameColor')]) . toHtml $ separator1
        jSmall entityNameColor' . toHtml $ separator2
        jSmall entityNameColor' $ middlePiece item
      jSmall timeWorkedColor' . toHtml . rightPiece $ item
  renderDetails (details item)

jEmptyGenericItem :: JoeThemeSettings -> String -> String -> GenericItem -> Html
jEmptyGenericItem themeSettings
  = jGenericItem themeSettings
      ((:[]) . leftText) middleText rightText False (\_ -> ()) $ \() -> do
        pure ()

jParagraphGenericItem :: JoeThemeSettings -> String -> String -> GenericItem -> Html
jParagraphGenericItem themeSettings
  = jGenericItem themeSettings
      ((:[]) . leftText) middleText rightText True paragraphs $ \paragraphs_ -> do
        let bodyColor' = bodyColor themeSettings
        let bodyFontSize = fontSize3 themeSettings
        forM_ paragraphs_ (jParagraph bodyColor' bodyFontSize)

jExperienceItem :: JoeThemeSettings -> String -> String -> ExperienceItem -> Html
jExperienceItem themeSettings
  = jGenericItem themeSettings
      positionName entityName timeWorked True Prelude.id $ \body -> do
        let bodyColor' = bodyColor themeSettings
        let bodyFontSize = fontSize3 themeSettings
        let timeWorkedColor' = timeWorkedColor themeSettings

        -- Display highlight if it exists
        forM_ (highlight body) $ \h -> do
          H.div ! applyStyles [("margin-bottom", "0.5em")] $ do -- Add some space below the highlight
            jParagraph bodyColor' bodyFontSize $ H.strong (fromString h)

        H.div ! applyStyles [("display", "table")] $ do
          forM_ [ ("Technologies", technologies)
                , ("Responsibilities", responsibilities)
                , ("Technological context", expertise)
                , ("Human context", contexts)
                , ("Extra-curricular", extraCurricular)
                ] $ \(lineName, getLine) -> do
            forM_ (getLine body) $ \s -> do
              H.div ! applyStyles [("display", "table-row")] $ do
                H.div ! applyStyles [ ("display", "table-cell")
                                    , ("width", "20ex")
                                    ] $ do
                  H.small $ H.i ! applyStyles [("color", timeWorkedColor')] $ lineName
                H.div ! applyStyles [("display", "table-cell")] $ do
                  jParagraph bodyColor' bodyFontSize $ do
                    H.span (fromString s)

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

jAISafetyItem :: JoeThemeSettings -> AISafetyItem -> Html
jAISafetyItem themeSettings item = H.div ! applyStyles sectionContainerStyles $ do
  let bodyColor' = bodyColor themeSettings
      timeWorkedColor' = timeWorkedColor themeSettings
      greyedColor = "#888888" -- A generic grey color for the URL

  -- Main content row (Description and Year)
  H.div ! applyStyles mainRowStyles $ do
    -- Description on the left
    H.span ! applyStyles [("color", bodyColor'), ("flex-grow", "1")] $ do
      toHtml (description item)

    -- Year on the right
    H.span ! applyStyles [("color", timeWorkedColor'), ("white-space", "nowrap"), ("margin-left", "1em")] $
      jSmall timeWorkedColor' (toHtml $ year item)

  -- Optional URL row (small, close, greyed-out)
  case url item of
    Nothing -> pure ()
    Just itemUrl -> do
      unless (null itemUrl) $ -- Check if the URL string is not empty
        H.div ! applyStyles urlRowStyles $
          H.small ! applyStyles [("color", greyedColor), ("font-size", "0.8em")] $
            toHtml itemUrl
  where
    sectionContainerStyles =
      [ ("display", "flex"),
        ("flex-direction", "column"),
        ("margin-bottom", "0.75em"),
        ("text-align", "left")
      ]
    mainRowStyles =
      [ ("display", "flex"),
        ("flex-direction", "row"),
        ("justify-content", "space-between"),
        ("align-items", "flex-start")
      ]
    urlRowStyles = []

-- Override the default line height to make it shorter.
shortLineHeight :: Html -> Html
shortLineHeight = H.div ! applyStyles
  [ ("line-height", "1em")
  ]