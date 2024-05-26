{-# LANGUAGE DeriveGeneric #-}
-- overloaded strings are enabled by the OverloadedStrings language pragma
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import CanvasData
import Control.Lens
import Data.Aeson
import Data.ByteString
import qualified Data.Text as Text
import Network.Wreq
import qualified Options.Applicative as Opt
import Pages (markdowntopage)

authAndPostPage :: BaseUrl -> CourseId -> Page -> AccessToken -> IO ()
authAndPostPage b c p a = do
  let ops = defaults & header "Authorization" .~ [append "Bearer " a] & header "Content-Type" .~ ["application/json"]
  let url = b <> "/courses/" <> c <> "/pages"
  let bdy = encode (WikiPage p)
  r <- postWith ops url bdy
  putStrLn $ show r

type CourseId = String
type BaseUrl = String
type AccessToken = ByteString

-- we put it all together we comman line arguments
-- for baseurl, courseid, accesstoken and the input file (-i)

data MyOptions = MyOptions
  { baseUrl :: BaseUrl
  , courseId :: CourseId
  , accessToken :: AccessToken
  , inputFile :: FilePath
  }

optionparser :: Opt.Parser MyOptions
optionparser =
  MyOptions
    <$> Opt.strOption
      ( Opt.long "baseurl"
          <> Opt.short 'b'
          <> Opt.help "The base url of the canvas instance"
          <> Opt.metavar "BASEURL"
      )
    <*> Opt.strOption
      ( Opt.long "courseid"
          <> Opt.short 'c'
          <> Opt.help "The course id"
          <> Opt.metavar "COURSEID"
      )
    <*> Opt.strOption
      ( Opt.long "accesstoken"
          <> Opt.short 'a'
          <> Opt.help "The access token"
          <> Opt.metavar "ACCESSTOKEN"
      )
    <*> Opt.strOption
      ( Opt.long "inputfile"
          <> Opt.short 'i'
          <> Opt.help "The input file"
          <> Opt.metavar "INPUTFILE"
      )

postPage :: IO ()
postPage = do
  opts <- Opt.execParser (Opt.info (optionparser Opt.<**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Post a Page to Canvas"))
  p <- markdowntopage (inputFile opts)
  authAndPostPage (baseUrl opts) (courseId opts) p (accessToken opts)
