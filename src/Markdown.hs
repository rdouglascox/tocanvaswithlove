{-# LANGUAGE OverloadedStrings #-}

module Markdown where

-- this module is going to use functions from the pandoc library
-- to convert a markdown file to the relevant formats
-- we want to push to the Canvas API.
-- we begin by important pandoc
--

import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Walk

-- a pandoc document consists of a meta data and a list of blocks
-- we want to write a function that walks all the blocks
-- and replaces string elements of the form %VAR% with the
-- the value of VAR from a map.
-- let's make sure we check the string for %VAR$

replaceVars :: [(T.Text, T.Text)] -> Inline -> Inline
replaceVars vars (Str s) =
  if T.isInfixOf "%" s
    then case lookup (T.dropAround (== '%') s) vars of
      Just v -> Str v
      Nothing -> Str s
    else Str s
replaceVars _ x = x

-- now we just want to walk the document and replace the vars

replaceVarsinDoc :: [(T.Text, T.Text)] -> Pandoc -> Pandoc
replaceVarsinDoc vars = walk (replaceVars vars)

-- here's an example map

egmap = [("VAR1", "value1"), ("VAR2", "value2")]

-- we read a markdown file and convert it to a Pandoc document
-- then we replace the vars in the document
-- and convert it to a string

egDoc = T.unlines ["# This is a test", "This is a %VAR1% and this is %VAR2%"]

doReplacePlaceholders :: IO T.Text
doReplacePlaceholders = do
  result <- runIO $ do
    doc <- readMarkdown def egDoc
    let newdoc = replaceVarsinDoc egmap doc
    writeMarkdown def newdoc
  md <- handleError result
  return (md)
