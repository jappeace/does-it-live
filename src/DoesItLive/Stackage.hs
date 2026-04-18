module DoesItLive.Stackage
  ( fetchStackagePackages
  , parseStackageConfig
  , fetchStackageLtsConfig
  , parseStackageVersions
  , formatProjectConstraints
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client (Manager, parseRequest, httpLbs, responseBody)

stackageNightlyUrl :: String
stackageNightlyUrl = "https://www.stackage.org/nightly/cabal.config"

-- | Fetch the set of package names included in Stackage nightly
fetchStackagePackages :: Manager -> IO (Set Text)
fetchStackagePackages manager = do
  request <- parseRequest stackageNightlyUrl
  response <- httpLbs request manager
  let body = Text.decodeUtf8 (LBS.toStrict (responseBody response))
  pure (parseStackageConfig body)

-- | Parse a Stackage cabal.config to extract package names.
-- Lines look like: "constraints: aeson ==2.1.2.1,"
-- or continuation: "             async ==2.2.5,"
parseStackageConfig :: Text -> Set Text
parseStackageConfig body =
  let linesList = Text.lines body
      extractPackageName line =
        let stripped = Text.strip line
            -- Remove leading "constraints:" if present
            withoutConstraints =
              case Text.stripPrefix "constraints:" stripped of
                Just rest -> Text.strip rest
                Nothing   -> stripped
            -- Remove trailing comma
            withoutComma = Text.dropWhileEnd (== ',') withoutConstraints
        in case Text.words withoutComma of
            (name : _) | not (Text.null name)
                       , Text.all (\c -> c /= '=' && c /= '-' || isPackageNameChar c) name
                       , isPackageNameChar (Text.head name)
                       -> Just name
            _          -> Nothing
  in Set.fromList [ name | line <- linesList
                         , not (Text.isPrefixOf "--" (Text.strip line))
                         , Just name <- [extractPackageName line]
                         ]

isPackageNameChar :: Char -> Bool
isPackageNameChar c = c >= 'a' && c <= 'z'
                   || c >= 'A' && c <= 'Z'
                   || c >= '0' && c <= '9'
                   || c == '-'

stackageLtsUrl :: String
stackageLtsUrl = "https://www.stackage.org/lts/cabal.config"

-- | Fetch Stackage LTS cabal.config as raw text
fetchStackageLtsConfig :: Manager -> IO Text
fetchStackageLtsConfig manager = do
  request <- parseRequest stackageLtsUrl
  response <- httpLbs request manager
  pure (Text.decodeUtf8 (LBS.toStrict (responseBody response)))

-- | Extract a package name -> version map from a Stackage cabal.config.
-- Lines look like: "constraints: aeson ==2.1.2.1," or "             async ==2.2.5,"
parseStackageVersions :: Text -> Map Text Text
parseStackageVersions body =
  let linesList = Text.lines body
  in Map.fromList
       [ (name, version)
       | line <- linesList
       , not (Text.isPrefixOf "--" (Text.strip line))
       , Just (name, version) <- [extractNameVersion line]
       ]
  where
    extractNameVersion :: Text -> Maybe (Text, Text)
    extractNameVersion line =
      let stripped = Text.strip line
          withoutConstraints =
            case Text.stripPrefix "constraints:" stripped of
              Just rest -> Text.strip rest
              Nothing   -> stripped
          withoutComma = Text.dropWhileEnd (== ',') withoutConstraints
      in case Text.words withoutComma of
          [name, versionConstraint]
            | not (Text.null name)
            , isPackageNameChar (Text.head name)
            , Text.all (\c -> c /= '=' && c /= '-' || isPackageNameChar c) name
            , Just version <- Text.stripPrefix "==" versionConstraint
            -> Just (name, version)
          _ -> Nothing

-- | Format a Stackage cabal.config into a cabal.project file.
-- Strips comment lines and with-compiler, prepends "packages: ."
formatProjectConstraints :: Text -> Text
formatProjectConstraints body =
  let linesList = Text.lines body
      isUsefulLine line =
        let stripped = Text.strip line
        in not (Text.isPrefixOf "--" stripped)
           && not (Text.isPrefixOf "with-compiler:" stripped)
           && not (Text.null stripped)
      constraintLines = filter isUsefulLine linesList
  in Text.unlines ("packages: ." : constraintLines)
