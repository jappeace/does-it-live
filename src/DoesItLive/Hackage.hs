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
-- Each row has: <td><a href="/package/PKG">PKG</a></td><td>TOTAL (...)</td><td>DIRECT (...)</td>
parseReverseDepsHtml :: Text -> Map Text Int
parseReverseDepsHtml html =
  let -- Find all table rows with package reverse dep data
      -- Pattern: /package/NAME/reverse/verbose">view</a>)</td><td>DIRECT_COUNT
      rows = extractReverseDepsRows html
  in Map.fromList rows

extractReverseDepsRows :: Text -> [(Text, Int)]
extractReverseDepsRows html =
  let chunks = drop 1 (Text.splitOn "/package/" html)
  in mapMaybe parseReverseDepChunk chunks

parseReverseDepChunk :: Text -> Maybe (Text, Int)
parseReverseDepChunk chunk =
  -- The chunk starts with "PKG/reverse/verbose">view</a>)</td><td>DIRECT (..."
  -- Or it starts with "PKG">PKG</a></td><td>TOTAL...</td><td>DIRECT..."
  case Text.breakOn "/reverse\">" chunk of
    (_, rest)
      | not (Text.null rest) ->
        -- This is the direct-count link, extract the package name from before it
        let nameWithSuffix = Text.takeWhile (/= '"') chunk
            -- nameWithSuffix might be "PKG/reverse/verbose" or just "PKG"
            pkgName = case Text.breakOn "/" nameWithSuffix of
              (name, _) -> name
        in case Text.breakOn ")</td><td>" rest of
             (_, afterTd)
               | not (Text.null afterTd) ->
                 let countArea = Text.drop (Text.length ")</td><td>") afterTd
                     countText = Text.takeWhile (\c -> c >= '0' && c <= '9') countText'
                     countText' = Text.strip countArea
                 in case Text.decimal countText of
                      Right (n, _) -> Just (pkgName, n)
                      Left _ -> Nothing
             _ -> Nothing
    _ -> Nothing

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
