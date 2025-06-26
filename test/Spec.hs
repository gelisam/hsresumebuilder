{-# LANGUAGE OverloadedStrings #-}

import ResumeBuilder.ResumeBuilderModel
import ResumeBuilder.Themes.JoeTheme.Components (jExperienceItem)
import ResumeBuilder.Themes.JoeTheme.Styler (applyStyles) -- Import if needed for jExperienceItem
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Maybe (fromMaybe)

-- Minimal JoeThemeSettings for testing
testThemeSettings :: JoeThemeSettings
testThemeSettings = JoeThemeSettings {
    bodyColor = "black",
    jobTitleColor = "blue",
    nameColor = "green",
    sectionTitlesColor = "red",
    sectionTitleBorderEnabled = False,
    entityNameColor = "purple",
    positionNameColor = "orange",
    timeWorkedColor = "cyan",
    linkColor = "magenta",
    bodyFontFamily = "Arial",
    titleFontFamily = "Times New Roman",
    fontSize1 = "24px",
    fontSize2 = "18px",
    fontSize3 = "12px",
    customStylesheetsToLoad = []
}

main :: IO ()
main = do
    putStrLn "Running tests..."
    testHighlightRendering
    putStrLn "Tests finished."

testHighlightRendering :: IO ()
testHighlightRendering = do
    let experienceWithHighlight = ExperienceItem {
            entityName = "Test Company",
            technologies = Just "Haskell, Nix",
            responsibilities = Just "Coding, Testing",
            expertise = Nothing,
            contexts = Nothing,
            extraCurricular = Nothing,
            positionName = ["Software Engineer"],
            timeWorked = "2023-Present",
            highlight = Just "This is a test highlight."
        }

    let experienceWithoutHighlight = ExperienceItem {
            entityName = "Another Company",
            technologies = Just "Python, Docker",
            responsibilities = Just "Development, Deployment",
            expertise = Nothing,
            contexts = Nothing,
            extraCurricular = Nothing,
            positionName = ["DevOps Engineer"],
            timeWorked = "2021-2022",
            highlight = Nothing
        }

    let htmlWithHighlight = renderHtml $ jExperienceItem testThemeSettings " " "at " experienceWithHighlight
    let htmlWithoutHighlight = renderHtml $ jExperienceItem testThemeSettings " " "at " experienceWithoutHighlight

    let highlightText = "This is a test highlight."

    if highlightText `isInfixOf` htmlWithHighlight
        then putStrLn "testHighlightRendering (with highlight): PASSED"
        else putStrLn "testHighlightRendering (with highlight): FAILED - Highlight not found in HTML."

    if not (highlightText `isInfixOf` htmlWithoutHighlight)
        then putStrLn "testHighlightRendering (without highlight): PASSED"
        else putStrLn "testHighlightRendering (without highlight): FAILED - Highlight found when it shouldn't be."

-- Helper function to check if a substring exists in a string
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (n:ns) (h:hs) = n == h && isPrefixOf ns hs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs
