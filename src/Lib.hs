{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Config
import Config (AnachronicThemeSections (shortIntroSectionBody))
import Control.Monad (forM_)
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

someFunc :: IO ()
someFunc = do
  maybeParsedConfig <- getConfig
  maybe exitFailure beginRendering maybeParsedConfig

beginRendering :: Preferences -> IO ()
beginRendering config = do
  let generatedContent = renderHtml . anachronicTheme $ config
  saveContentToFile generatedContent
  putStrLn generatedContent

saveContentToFile :: String -> IO ()
saveContentToFile content = do
  handle <- openFile "output.html" ReadWriteMode
  contents <- hGetContents handle
  hClose handle
  writeFile "output.html" content

anachronicTheme :: Preferences -> Html
anachronicTheme config = docTypeHtml $ do
  H.head $ do
    H.title . toHtml $ ("Resume of " ++ (displayName . personalInformation $ config))
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css"
  body $ do
    main $ do
      h1 . toHtml . displayName . personalInformation $ config
      (h2 ! A.style "color: darkgreen;") . toHtml . jobTitle . personalInformation $ config
      H.div ! A.style "display: flex" $ do
        H.div ! A.style "padding: 1em;" $ do
          forM_ (phoneNumbers . contactInformation . personalInformation $ config) (p . toHtml)
          forM_ (emails . contactInformation . personalInformation $ config) (p . toHtml)
          forM_ (websites . contactInformation . personalInformation $ config) (p . toHtml)
        H.div ! A.style "padding: 1em;" $ do
          forM_ (addressLines . personalInformation $ config) (p . toHtml)

      H.div $ do
        h3 . toHtml $ (shortIntroSectionTitle . themeSections . appearancePreferences $ config)
        (p ! A.style "text-align: justify") . toHtml $ (shortIntroSectionBody . themeSections . appearancePreferences $ config)
