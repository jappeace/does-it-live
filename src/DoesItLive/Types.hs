module DoesItLive.Types
  ( PackageInfo(..)
  , ScoreResult(..)
  , ScoreComponents(..)
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

data ScoreResult = ScoreResult
  { scorePackageName :: Text
  , totalScore       :: Int
  , components       :: ScoreComponents
  } deriving stock (Show, Eq, Generic)

instance ToNamedRecord ScoreResult where
  toNamedRecord ScoreResult{scorePackageName, totalScore, components} =
    namedRecord
      [ "package"       .= scorePackageName
      , "total"         .= totalScore
      , "recency"       .= recencyScore components
      , "stackage"      .= stackageScore components
      , "not_deprecated" .= deprecatedScore components
      , "reverse_deps"  .= reverseDepsScore components
      , "versions"      .= versionScore components
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
    ]
