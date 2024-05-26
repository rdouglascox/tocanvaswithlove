{-# LANGUAGE DeriveGeneric #-}
-- overloaded strings are enabled by the OverloadedStrings language pragma
{-# LANGUAGE OverloadedStrings #-}

module CanvasData where

import Data.Aeson
import GHC.Generics

data Page = Page {title :: String, body :: String, published :: Bool}
  deriving (Show, Generic)

data WikiPage = WikiPage {wiki_page :: Page}
  deriving (Show, Generic)

instance ToJSON Page
instance FromJSON Page

instance ToJSON WikiPage
instance FromJSON WikiPage
