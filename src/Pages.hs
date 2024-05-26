{-# LANGUAGE OverloadedStrings #-}

module Pages (markdowntopage) where

-- this module takes a markdown file and uses pandoc
-- to convert it to pair with a title for the page and a string
-- represing the html content of the page

import CanvasData
import Data.Text
import Data.Text.IO as T
import Text.Pandoc

markdowntopage :: FilePath -> IO Page
markdowntopage path = do
  filecontent <- T.readFile path
  result <- runIOorExplode $ do
    markdown <- readMarkdown def{readerStandalone = True, readerExtensions = enableExtension Ext_yaml_metadata_block pandocExtensions} filecontent
    let ttl = gettitle markdown
    cont <- writeHtml5String def markdown
    return (Page (unpack ttl) (unpack cont) True)
  return result

gettitle :: Pandoc -> Text
gettitle (Pandoc meta _) = case lookupMeta "title" meta of
  Just (MetaString s) -> s
  Just (MetaInlines ils) -> stringify ils
  Just _ -> "No title"
  Nothing -> "No title"

stringify :: [Inline] -> Text
stringify [] = ""
stringify (Str s : xs) = s <> stringify xs
stringify (Space : xs) = " " <> stringify xs
stringify (LineBreak : xs) = "\n" <> stringify xs
stringify (_ : xs) = stringify xs
