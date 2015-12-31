{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Aeson as A
import Data.ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe
import Data.Word (Word16)
import GHC.Generics (Generic)
import Network.Http.Client
import OpenSSL

serverUrl :: ByteString
serverUrl = "api.github.com"

serverPort :: Word16
serverPort = 443

repoUrl :: ByteString
repoUrl = "/users/rcook/repos"

data Repo = Repo {
  name :: String
} deriving (Show, Generic)

instance A.FromJSON Repo

main :: IO ()
main = withOpenSSL $ do
  ctx <- baselineContextSSL
  withConnection (openConnectionSSL ctx serverUrl serverPort) $ \c -> do
    request <- buildRequest $ do
      http GET repoUrl
      setAccept "application/json"
      setHeader "User-Agent" "rcookGitHubApiClient"
    sendRequest c request emptyBody
    json <- receiveResponse c concatHandler
    let repos = fromJust (A.decode $ Lazy.pack $ unpack json :: Maybe [Repo])
    forM_ repos $ \repo -> print repo

