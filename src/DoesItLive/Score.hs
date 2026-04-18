module DoesItLive.Score
  ( scorePackage
  , scoreRecency
  , scoreStackage
  , scoreDeprecated
  , scoreReverseDeps
  , scoreVersionCount
  ) where

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import DoesItLive.Types (PackageInfo(..), ScoreResult(..), ScoreComponents(..), noBuildChecked)

-- | Score a package on a 0-100 scale based on maintenance signals
scorePackage :: UTCTime -> PackageInfo -> ScoreResult
scorePackage now info =
  let comps = ScoreComponents
        { recencyScore    = scoreRecency now (lastUpload info)
        , stackageScore   = scoreStackage (inStackage info)
        , deprecatedScore = scoreDeprecated (isDeprecated info)
        , reverseDepsScore = scoreReverseDeps (reverseDepCount info)
        , versionScore    = scoreVersionCount (versionCount info)
        }
      total = recencyScore comps
            + stackageScore comps
            + deprecatedScore comps
            + reverseDepsScore comps
            + versionScore comps
  in ScoreResult
      { scorePackageName = packageName info
      , totalScore       = total
      , components       = comps
      , buildStatus      = noBuildChecked
      }

-- | Upload recency: max 30 points
-- <6mo -> 30, <1y -> 25, <2y -> 20, <3y -> 15, <5y -> 10, <10y -> 5, else -> 0
scoreRecency :: UTCTime -> Maybe UTCTime -> Int
scoreRecency _ Nothing = 0
scoreRecency now (Just uploaded) =
  let ageSeconds :: NominalDiffTime
      ageSeconds = diffUTCTime now uploaded
      sixMonths = 182.5 * 86400
      oneYear   = 365 * 86400
      twoYears  = 730 * 86400
      threeYears = 1095 * 86400
      fiveYears = 1825 * 86400
      tenYears  = 3650 * 86400
  in if ageSeconds < sixMonths then 30
     else if ageSeconds < oneYear then 25
     else if ageSeconds < twoYears then 20
     else if ageSeconds < threeYears then 15
     else if ageSeconds < fiveYears then 10
     else if ageSeconds < tenYears then 5
     else 0

-- | Stackage nightly inclusion: 25 points if included
scoreStackage :: Bool -> Int
scoreStackage True  = 25
scoreStackage False = 0

-- | Not deprecated: 10 points if not deprecated
scoreDeprecated :: Bool -> Int
scoreDeprecated True  = 0
scoreDeprecated False = 10

-- | Reverse dependency count: max 20 points
-- min 20 (round (logBase 2 (count + 1) * 3))
scoreReverseDeps :: Int -> Int
scoreReverseDeps count =
  min 20 (round (logBase 2 (fromIntegral count + 1 :: Double) * 3))

-- | Version count: max 15 points
-- 10+ -> 15, 5-9 -> 10, 2-4 -> 5, 1 -> 2
scoreVersionCount :: Int -> Int
scoreVersionCount count
  | count >= 10 = 15
  | count >= 5  = 10
  | count >= 2  = 5
  | count >= 1  = 2
  | otherwise   = 0
