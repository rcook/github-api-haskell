{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString
import Data.Word (Word16)
import Network.Http.Client
import OpenSSL
import System.IO.Streams (stdout)
import qualified System.IO.Streams as Streams

serverUrl :: ByteString
serverUrl = "api.github.com"

serverPort :: Word16
serverPort = 443

repoUrl :: ByteString
repoUrl = "/users/rcook/repos"

main :: IO ()
main = withOpenSSL $ do
  ctx <- baselineContextSSL
  withConnection (openConnectionSSL ctx serverUrl serverPort) $ \c -> do
    request <- buildRequest $ do
      http GET repoUrl
      setAccept "application/json"
      setHeader "User-Agent" "rcookGitHubApiClient"
    sendRequest c request emptyBody
    receiveResponse c $ \_ i -> Streams.connect i stdout

