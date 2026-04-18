module DoesItLive.Types
  ( PackageInfo(..)
  , ScoreResult(..)
  , ScoreComponents(..)
  , BuildStatus(..)
  , BuildOutcome(..)
  , noBuildChecked
  , renderTriState
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

-- | Outcome of a build attempt
data BuildOutcome = BuildOutcome
  { constraintsSolved :: Bool
  , buildSucceeded    :: Bool
  , usedJailbreak     :: Bool
  } deriving stock (Show, Eq, Generic)

-- | Whether a package was build-checked and the result
data BuildStatus
  = BuildChecked BuildOutcome
  | BuildNotChecked
  deriving stock (Show, Eq, Generic)

-- | Default unchecked status
noBuildChecked :: BuildStatus
noBuildChecked = BuildNotChecked

-- | Render a Bool for CSV, empty when not checked
renderTriState :: (BuildStatus -> Maybe Bool) -> BuildStatus -> Text
renderTriState _ BuildNotChecked = ""
renderTriState extract (BuildChecked outcome) =
  case extract (BuildChecked outcome) of
    Just True  -> "yes"
    Just False -> "no"
    Nothing    -> ""

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
      , "can_solve"     .= renderCanSolve buildStatus
      , "builds"        .= renderBuilds buildStatus
      , "jailbroken"    .= renderJailbroken buildStatus
      ]

renderCanSolve :: BuildStatus -> Text
renderCanSolve BuildNotChecked = ""
renderCanSolve (BuildChecked outcome) =
  if constraintsSolved outcome then "yes" else "no"

renderBuilds :: BuildStatus -> Text
renderBuilds BuildNotChecked = ""
renderBuilds (BuildChecked outcome) =
  if buildSucceeded outcome then "yes" else "no"

renderJailbroken :: BuildStatus -> Text
renderJailbroken BuildNotChecked = ""
renderJailbroken (BuildChecked outcome) =
  if usedJailbreak outcome then "yes" else "no"

instance DefaultOrdered ScoreResult where
  headerOrder _ = header
    [ "package"
    , "total"
    , "recency"
    , "stackage"
    , "not_deprecated"
    , "reverse_deps"
    , "versions"
    , "can_solve"
    , "builds"
    , "jailbroken"
    ]
