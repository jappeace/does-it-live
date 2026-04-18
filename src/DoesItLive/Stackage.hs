module DoesItLive.Stackage
  ( fetchStackagePackages
  , parseStackageConfig
  ) where

import Data.ByteString.Lazy qualified as LBS
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
