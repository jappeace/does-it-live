module DoesItLive.Types
  ( PackageInfo(..)
  , ScoreResult(..)
  , ScoreComponents(..)
  , BuildStatus(..)
  , renderBuildStatus
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Csv (ToNamedRecord(..), namedRecord, (.=), DefaultOrdered(..), header)
import GHC.Generics (Generic)

data PackageInfo = PackageInfo
  { packageName     :: Text
  , versionCount    :: Int
  , lastUpload      :: Maybe UTCTime
  , isDeprecated    :: Bool
  , reverseDepCount :: Int
  , inStackage      :: Bool
  } deriving stock (Show, Eq, Generic)

data ScoreComponents = ScoreComponents
  { recencyScore    :: Int
  , stackageScore   :: Int
  , deprecatedScore :: Int
  , reverseDepsScore :: Int
  , versionScore    :: Int
  } deriving stock (Show, Eq, Generic)

-- | Whether a package was build-checked and the result
data BuildStatus
  = BuildChecked Bool
  | BuildNotChecked
  deriving stock (Show, Eq, Generic)

-- | Render build status for CSV output
renderBuildStatus :: BuildStatus -> Text
renderBuildStatus (BuildChecked True)  = "yes"
renderBuildStatus (BuildChecked False) = "no"
renderBuildStatus BuildNotChecked      = ""

data ScoreResult = ScoreResult
  { scorePackageName :: Text
  , totalScore       :: Int
  , components       :: ScoreComponents
  , buildStatus      :: BuildStatus
  } deriving stock (Show, Eq, Generic)

instance ToNamedRecord ScoreResult where
  toNamedRecord ScoreResult{scorePackageName, totalScore, components, buildStatus} =
    namedRecord
      [ "package"       .= scorePackageName
      , "total"         .= totalScore
      , "recency"       .= recencyScore components
      , "stackage"      .= stackageScore components
      , "not_deprecated" .= deprecatedScore components
      , "reverse_deps"  .= reverseDepsScore components
      , "versions"      .= versionScore components
      , "builds"        .= renderBuildStatus buildStatus
      ]

instance DefaultOrdered ScoreResult where
  headerOrder _ = header
    [ "package"
    , "total"
    , "recency"
    , "stackage"
    , "not_deprecated"
    , "reverse_deps"
    , "versions"
    , "builds"
    ]
