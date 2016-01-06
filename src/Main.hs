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

-- | Returns True if URI requires SSL or False otherwise
--
-- Examples:
--
-- >>> uriIsSsl $ fromJust $ parseURI "https://example.com"
-- True
-- >>> uriIsSsl $ fromJust $ parseURI "http://example.com"
-- False
-- >>> uriIsSsl nullURI
-- False
uriIsSsl :: URI -> Bool
uriIsSsl uri = uriScheme uri == "https:"

-- | Gets host name from given URI
--
-- Examples:
-- >>> uriGetHostName $ fromJust $ parseURI "https://www.example.com/a/b"
-- Just "www.example.com"
-- >>> uriGetHostName nullURI
-- Nothing
uriGetHostName :: URI -> Maybe String
uriGetHostName uri = uriRegName <$> uriAuthority uri

-- | Gets port number from given URI
--
-- Examples:
-- >>> uriGetPort (fromJust $ parseURI "https://www.example.com/a/b") 1234
-- Just 1234
-- >>> uriGetPort (fromJust $ parseURI "https://www.example.com:4567/a/b") 1234
-- Just 4567
-- >>> uriGetPort nullURI 1234
-- Nothing
uriGetPort :: URI -> Word16 -> Maybe Word16
uriGetPort uri defaultPort = do
  auth <- uriAuthority uri
  return $ case uriPort auth of
                "" -> defaultPort
                p -> (Prelude.read $ Prelude.tail p) :: Word16

-- | Gets full path from given URI
--
-- Examples:
-- >>> uriGetFullPath $ fromJust $ parseURI "https://www.example.com/a/b"
-- "/a/b"
-- >>> uriGetFullPath $ fromJust $ parseURI "https://www.example.com"
-- ""
-- >>> uriGetFullPath $ fromJust $ parseURI "https://www.example.com/"
-- "/"
-- >>> uriGetFullPath nullURI
-- ""
uriGetFullPath :: URI -> String
uriGetFullPath uri = uriPath uri ++ uriQuery uri ++ uriFragment uri

-- | Returns True if link has parameter with given value or False otherwise
--
-- Examples:
-- >>> :set -XOverloadedStrings
-- >>> let link = Link (fromJust $ parseURI "https://example.com/hello%20world") [(Rel, "next"), (Title, "hello world")]
-- >>> containsLinkParam link Rel "prev"
-- False
-- >>> containsLinkParam link Rel "next"
-- True
-- >>> containsLinkParam link Title "goodbye"
-- False
-- >>> containsLinkParam link Title "hello world"
-- True
containsLinkParam :: Link -> LinkParam -> Text -> Bool
containsLinkParam link linkParam value =
  isJust $ L.find (\(lp, v) -> lp == linkParam && v == value) $ linkParams link

-- | Returns True if link has Rel="next" link parameter or False otherwise
--
-- Examples:
-- >>> :set -XOverloadedStrings
-- >>> let link0 = Link (fromJust $ parseURI "https://example.com/hello%20world") [(Rel, "next"), (Title, "hello world")]
-- >>> let link1 = Link (fromJust $ parseURI "https://example.com/hello%20world") [(Rel, "prev"), (Title, "hello world")]
-- >>> hasRelNext link0
-- True
-- >>> hasRelNext link1
-- False
hasRelNext :: Link -> Bool
hasRelNext link = containsLinkParam link Rel "next"

-- | Parses a link header and returns a Rel="next" link if it exists
--
-- Examples:
-- >>> :set -XOverloadedStrings
-- >>> findNextLink "<https://example.com/2>; rel=\"next\", <https://example.com/0>; rel=prev"
-- Just (Link https://example.com/2 [(Rel,"next")])
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

