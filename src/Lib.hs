{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Config (PersonalInfo (addressLines, contactInformation, jobTitle), PersonalInfoContactInfo (emails, phoneNumbers, websites), Preferences (Preferences, personalInformation), displayName, getConfig)
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
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"
  body $ do
    h1 . toHtml . displayName . personalInformation $ config
    (h2 ! A.style "color: darkgreen;") . toHtml . jobTitle . personalInformation $ config
    H.div $ do
      ul $ forM_ (phoneNumbers . contactInformation . personalInformation $ config) (p . toHtml)
      ul $ forM_ (emails . contactInformation . personalInformation $ config) (p . toHtml)
      ul $ forM_ (websites . contactInformation . personalInformation $ config) (p . toHtml)
    H.div $ do
      ul
