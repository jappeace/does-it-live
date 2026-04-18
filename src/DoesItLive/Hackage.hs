module DoesItLive.Hackage
  ( fetchPackageNames
  , fetchDeprecated
  , fetchReverseDeps
  , fetchPackageVersions
  , fetchUploadTime
  , parseReverseDepsHtml
  , parseUploadTimeHtml
  , hackageJsonRequest
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Read qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager, parseRequest, httpLbs, responseBody, requestHeaders )

hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"

-- | Make a request to Hackage with JSON Accept header
hackageJsonRequest :: Manager -> String -> IO LBS.ByteString
hackageJsonRequest manager url = do
  initialRequest <- parseRequest url
  let request = initialRequest
        { requestHeaders = [("Accept", "application/json")] }
  responseBody <$> httpLbs request manager

-- | Make a request to Hackage without JSON header (for HTML)
hackageHtmlRequest :: Manager -> String -> IO LBS.ByteString
hackageHtmlRequest manager url = do
  request <- parseRequest url
  responseBody <$> httpLbs request manager

-- | Wrapper for the package list JSON response
newtype PackageEntry = PackageEntry { packageEntryName :: Text }
  deriving stock (Show, Generic)

instance FromJSON PackageEntry where
  parseJSON = withObject "PackageEntry" $ \obj ->
    PackageEntry <$> obj .: "packageName"

-- | Fetch all package names from Hackage
fetchPackageNames :: Manager -> IO [Text]
fetchPackageNames manager = do
  body <- hackageJsonRequest manager (hackageBaseUrl <> "/packages/")
  case eitherDecode body of
    Left err -> fail ("Failed to parse package list: " <> err)
    Right entries -> pure (map packageEntryName entries)

-- | Wrapper for deprecated package entries
newtype DeprecatedEntry = DeprecatedEntry { deprecatedName :: Text }
  deriving stock (Show, Generic)

instance FromJSON DeprecatedEntry where
  parseJSON = withObject "DeprecatedEntry" $ \obj ->
    DeprecatedEntry <$> obj .: "deprecated-package"

-- | Fetch set of deprecated package names
fetchDeprecated :: Manager -> IO (Set Text)
fetchDeprecated manager = do
  body <- hackageJsonRequest manager (hackageBaseUrl <> "/packages/deprecated")
  case eitherDecode body of
    Left err -> fail ("Failed to parse deprecated list: " <> err)
    Right entries -> pure (Set.fromList (map deprecatedName entries))

-- | Fetch reverse dependency counts by parsing the /packages/reverse HTML page.
-- Returns a map from package name to direct reverse dep count.
fetchReverseDeps :: Manager -> IO (Map Text Int)
fetchReverseDeps manager = do
  body <- hackageHtmlRequest manager (hackageBaseUrl <> "/packages/reverse")
  let bodyText = Text.decodeUtf8 (LBS.toStrict body)
  pure (parseReverseDepsHtml bodyText)

-- | Parse the reverse deps HTML table.
-- Each row looks like:
-- <tr class="odd"><td><a href="/package/PKG">PKG</a></td><td>TOTAL (...)</td><td>DIRECT (...)</td></tr>
-- We extract PKG name and DIRECT count (the number in the third <td>).
parseReverseDepsHtml :: Text -> Map Text Int
parseReverseDepsHtml html =
  let rows = drop 1 (Text.splitOn "<tr class=\"" html)
  in Map.fromList (mapMaybe parseReverseDepRow rows)

parseReverseDepRow :: Text -> Maybe (Text, Int)
parseReverseDepRow row = do
  -- Extract package name from: ...><td><a href="/package/PKG">PKG</a></td>...
  let (_, afterFirstTd) = Text.breakOn "<td><a href=\"/package/" row
  rest1 <- Text.stripPrefix "<td><a href=\"/package/" afterFirstTd
  let (_, afterClose) = Text.breakOn "\">" rest1
  rest2 <- Text.stripPrefix "\">" afterClose
  let pkgName = Text.takeWhile (/= '<') rest2
  -- Skip first </td><td> (total count) and find the direct count in the third <td>
  -- Structure: ...PKG</a></td><td>TOTAL (...)</td><td>DIRECT (...)</td>
  let (_, afterSecondTd) = Text.breakOn "</td><td>" rest2
  rest3 <- Text.stripPrefix "</td><td>" afterSecondTd
  -- rest3 starts with "TOTAL (..."
  let (_, afterThirdTd) = Text.breakOn "</td><td>" rest3
  rest4 <- Text.stripPrefix "</td><td>" afterThirdTd
  -- rest4 starts with "DIRECT (..."
  let countText = Text.takeWhile (\c -> c >= '0' && c <= '9') rest4
  case Text.decimal countText of
    Right (n, _) -> Just (pkgName, n)
    Left _ -> Nothing

-- | Fetch version info for a single package.
-- Returns a map from version string to status ("normal" or "deprecated").
fetchPackageVersions :: Manager -> Text -> IO (Map Text Text)
fetchPackageVersions manager name = do
  body <- hackageJsonRequest manager
    (hackageBaseUrl <> "/package/" <> Text.unpack name)
  case eitherDecode body of
    Left _err ->
      -- Some packages may fail to parse, return empty
      pure Map.empty
    Right versions -> pure versions

-- | Fetch the upload time of the latest version of a package by parsing HTML.
-- Fetches the package page for a specific version and extracts the upload timestamp.
fetchUploadTime :: Manager -> Text -> Text -> IO (Maybe UTCTime)
fetchUploadTime manager name version = do
  body <- hackageHtmlRequest manager
    (hackageBaseUrl <> "/package/" <> Text.unpack name <> "-" <> Text.unpack version)
  let bodyText = Text.decodeUtf8 (LBS.toStrict body)
  pure (parseUploadTimeHtml bodyText)

-- | Parse upload time from package version HTML page.
-- Looks for: <span title="...">ISO8601_TIMESTAMP</span>
parseUploadTimeHtml :: Text -> Maybe UTCTime
parseUploadTimeHtml html =
  -- The upload time appears as: <span title="...">2026-04-16T20:18:57Z</span>
  -- after the "Uploaded" table header
  case Text.breakOn "<th>Uploaded</th>" html of
    (_, rest)
      | not (Text.null rest) ->
        -- Find the ISO timestamp in the <span> after this
        case Text.breakOn "\">" (snd (Text.breakOn "<span title=\"" rest)) of
          (_, afterQuote)
            | not (Text.null afterQuote) ->
              let timestampArea = Text.drop 2 afterQuote
                  timestamp = Text.takeWhile (/= '<') timestampArea
              in parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                   (Text.unpack timestamp)
          _ -> Nothing
    _ -> Nothing
