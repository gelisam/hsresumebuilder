{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Config
import Control.Monad (forM_)
import Data.String
import JoeTheme (joeTheme)
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
  let generatedContent = renderHtml . joeTheme $ config
  saveContentToFile generatedContent
  putStrLn generatedContent

saveContentToFile :: String -> IO ()
saveContentToFile content = do
  handle <- openFile "output.html" ReadWriteMode
  contents <- hGetContents handle
  hClose handle
  writeFile "output.html" content
