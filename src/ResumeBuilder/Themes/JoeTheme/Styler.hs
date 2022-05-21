module ResumeBuilder.Themes.JoeTheme.Styler where

import Data.String (IsString (fromString))
import Text.Blaze.Html5 as H (Attribute)
import Text.Blaze.Html5.Attributes as A (class_, style)

type StyleAttribute = String

type StyleAttributeValue = String

type Style = (StyleAttribute, StyleAttributeValue)

type Styles = [Style]

type Class = String

type Classes = [Class]

getAttributeValuePairs :: Styles -> [(String, String)]
getAttributeValuePairs valuePairs = valuePairs

showStyles :: Styles -> String
showStyles styles = toString
  where
    toString = concatMap (\(k, v) -> k ++ ": " ++ v ++ ";") styles

showClasses :: Classes -> String
showClasses classes = toString
  where
    toString = concatMap (++ " ") classes

applyStyles :: Styles -> Attribute
applyStyles = A.style . fromString . showStyles

applyClasses :: Classes -> Attribute
applyClasses = A.class_ . fromString . showClasses