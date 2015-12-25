{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import Data.Maybe
import Data.Text
import qualified Data.Text.Encoding as E
import Data.Word (Word16)
import GHC.Generics (Generic)
import Network.Http.Client
import Network.HTTP.Link
import Network.URI
import OpenSSL

uriIsSsl :: URI -> Bool
uriIsSsl uri = uriScheme uri == "https:"

uriGetHostName :: URI -> Maybe String
uriGetHostName uri = uriRegName <$> uriAuthority uri

uriGetPort :: URI -> Word16 -> Maybe Word16
uriGetPort uri defaultPort = do
  auth <- uriAuthority uri
  return $ case uriPort auth of
                "" -> defaultPort
                p -> (Prelude.read $ Prelude.tail p) :: Word16

uriGetFullPath :: URI -> String
uriGetFullPath uri = uriPath uri ++ uriQuery uri ++ uriFragment uri

containsLinkParam :: Link -> LinkParam -> Text -> Bool
containsLinkParam link linkParam value =
  isJust $ L.find (\(lp, v) -> lp == linkParam && v == value) $ linkParams link

hasRelNext :: Link -> Bool
hasRelNext link = containsLinkParam link Rel "next"

findNextLink :: BS.ByteString -> Maybe Link
findNextLink value = do
  links <- parseLinkHeader $ E.decodeUtf8 value
  L.find hasRelNext links

getLinkHeader :: Response -> Maybe BS.ByteString
getLinkHeader p = getHeader p "Link"

nextLinkFromResponse :: Response -> Maybe Link
nextLinkFromResponse p = getLinkHeader p >>= findNextLink

openUri :: URI -> (Connection -> BS.ByteString -> IO a) -> IO a
openUri uri f =
  let
    isSsl = uriIsSsl uri
    hostName = C8.pack $ fromJust $ uriGetHostName uri
    port = fromJust $ uriGetPort uri (if isSsl then 443 else 80)
    fullPath = C8.pack $ uriGetFullPath uri
    wrappedF c = f c fullPath
  in
    if isSsl
       then
          withOpenSSL $ do
            ctx <- baselineContextSSL
            withConnection (openConnectionSSL ctx hostName port) wrappedF
       else
          withConnection (openConnection hostName port) wrappedF

data Repo = Repo {
    name :: String
  , description :: String
  , language :: Maybe String
} deriving (Show, Generic)

instance FromJSON Repo

fetchRepos :: URI -> IO [Repo]
fetchRepos uri =
  openUri uri $ \c fullPath -> do
    request <- buildRequest $ do
      http GET fullPath
      setAccept "application/json"
      setHeader "User-Agent" "rcookGitHubApiClient"
    sendRequest c request emptyBody
    receiveResponse c $ \p i -> do
      repos <- jsonHandler p i
      nextRepos <- case nextLinkFromResponse p of
                        Just link -> fetchRepos $ href link
                        Nothing -> return []
      return $ repos ++ nextRepos

main :: IO ()
main = do
  repos <- fetchRepos $ fromJust $ parseURI "https://api.github.com/users/rcook/repos"
  putStrLn $ show (Prelude.length repos) ++ " repos:"
  forM_ repos $ \repo -> print repo

