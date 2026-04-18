module DoesItLive
  ( runScorer
  , Options(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..), comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO (hPutStrLn, hFlush, stderr)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Process.Typed (proc, readProcess_)

import DoesItLive.Build (BuildResult(..), attemptBuild)
import DoesItLive.Hackage
  ( fetchPackageNames, fetchDeprecated, fetchReverseDeps
  , fetchPackageVersions, fetchUploadTime )
import DoesItLive.Score (scorePackage)
import DoesItLive.Stackage
  ( fetchStackagePackages, fetchStackageLtsConfig
  , parseStackageVersions, formatProjectConstraints )
import DoesItLive.Types (PackageInfo(..), ScoreResult(..), BuildStatus(..), BuildOutcome(..))

data Options = Options
  { optOutput       :: FilePath
  , optConcurrency  :: Int
  , optMinScore     :: Int
  , optMaxScore     :: Int
  , optCheckBuilds  :: Bool
  , optBuildTimeout :: Int
  } deriving stock (Show)

-- | Run the full scoring pipeline, returning scored results sorted by score descending
runScorer :: Options -> IO [ScoreResult]
runScorer opts = do
  manager <- newTlsManager
  now <- getCurrentTime

  -- Phase 1: Fetch bulk data concurrently
  logMsg "Fetching package list..."
  packageNames <- fetchPackageNames manager

  logMsg ("Found " <> show (length packageNames) <> " packages")
  logMsg "Fetching bulk data (deprecated, reverse deps, stackage)..."

  deprecatedSet <- fetchDeprecated manager
  logMsg ("  Deprecated: " <> show (Set.size deprecatedSet) <> " packages")

  reverseDepsMap <- fetchReverseDeps manager
  logMsg ("  Reverse deps data: " <> show (Map.size reverseDepsMap) <> " packages")

  stackageSet <- fetchStackagePackages manager
  logMsg ("  Stackage nightly: " <> show (Set.size stackageSet) <> " packages")

  -- Phase 2: Fetch per-package version data with throttling
  logMsg "Fetching per-package version info..."
  let totalPackages = length packageNames
  completedRef <- newIORef (0 :: Int)

  let fetchOnePackage :: Text -> IO PackageInfo
      fetchOnePackage name = do
        versions <- fetchVersionsSafe manager name
        let versionList = Map.keys versions
            latestVersion = findLatestVersion versionList
        uploadTime <- case latestVersion of
          Just ver -> fetchUploadTimeSafe manager name ver
          Nothing  -> pure Nothing

        completed <- atomicModifyIORef' completedRef (\n -> (n + 1, n + 1))
        if completed `mod` 500 == 0
          then logMsg ("  Progress: " <> show completed <> "/" <> show totalPackages)
          else pure ()

        pure PackageInfo
          { packageName     = name
          , versionCount    = Map.size versions
          , lastUpload      = uploadTime
          , isDeprecated    = Set.member name deprecatedSet
          , reverseDepCount = fromMaybe 0 (Map.lookup name reverseDepsMap)
          , inStackage      = Set.member name stackageSet
          }

  -- Process in batches to respect rate limiting
  packageInfos <- batchProcess (optConcurrency opts) fetchOnePackage packageNames

  -- Phase 3: Score all packages
  logMsg "Scoring packages..."
  let results = map (scorePackage now) packageInfos
      filtered = filter (\r -> totalScore r >= optMinScore opts
                              && totalScore r <= optMaxScore opts) results
      sorted = sortBy (comparing (Down . totalScore)) filtered

  logMsg ("Done! " <> show (length sorted) <> " packages scored above threshold "
         <> show (optMinScore opts))

  -- Phase 4: Optional build checking
  if optCheckBuilds opts
    then do
      logMsg "Phase 4: Checking builds..."
      logMsg "Fetching Stackage LTS constraints..."
      ltsConfig <- fetchStackageLtsConfig manager
      let versionMap = parseStackageVersions ltsConfig
          ltsProjectContent = formatProjectConstraints ltsConfig
          bareProjectContent = "packages: .\n" :: Text

      logMsg ("  LTS has " <> show (Map.size versionMap) <> " packages")

      logMsg "Running cabal update..."
      _ <- readProcess_ $ proc "cabal" ["update"]

      tmpDir <- getTemporaryDirectory
      let buildDir = tmpDir <> "/does-it-live-builds"
      createDirectoryIfMissing True buildDir

      let totalToBuild = length sorted
      buildResults <- mapM (\(idx, result) -> do
        let name = scorePackageName result
            (version, projectContent) = case Map.lookup name versionMap of
              Just ver -> (Just ver, ltsProjectContent)
              Nothing  -> (Nothing, bareProjectContent)
            label = Text.unpack name <> case version of
              Just ver -> "-" <> Text.unpack ver
              Nothing  -> " (latest)"
        logMsg ("  [" <> show idx <> "/" <> show totalToBuild
               <> "] " <> label <> ": building...")
        buildResult <- attemptBuild buildDir projectContent name version (optBuildTimeout opts)
        let status = case buildResult of
              BuildAttempted outcome -> BuildChecked outcome
              BuildTimeout -> BuildChecked BuildOutcome
                { constraintsSolved = False
                , buildSucceeded    = False
                , usedJailbreak     = False
                }
              BuildGetFailed _ -> BuildChecked BuildOutcome
                { constraintsSolved = False
                , buildSucceeded    = False
                , usedJailbreak     = False
                }
        logMsg ("      -> " <> showBuildResult buildResult)
        pure result { buildStatus = status }
        ) (zip [(1 :: Int)..] sorted)
      pure buildResults
    else pure sorted

-- | Fetch versions with error handling
fetchVersionsSafe :: Manager -> Text -> IO (Map.Map Text Text)
fetchVersionsSafe manager name = do
  result <- try (fetchPackageVersions manager name)
  case result of
    Left (_ :: SomeException) -> pure Map.empty
    Right versions -> pure versions

-- | Fetch upload time with error handling
fetchUploadTimeSafe :: Manager -> Text -> Text -> IO (Maybe UTCTime)
fetchUploadTimeSafe manager name version = do
  result <- try (fetchUploadTime manager name version)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right uploadTime -> pure uploadTime

-- | Find the latest version by simple text sorting of version strings.
-- This isn't perfect but works well enough for most Haskell packages.
findLatestVersion :: [Text] -> Maybe Text
findLatestVersion [] = Nothing
findLatestVersion versions =
  Just (maximum (map normalizeVersion versions))
  where
    normalizeVersion :: Text -> Text
    normalizeVersion = id

-- | Process items in batches with concurrency limit and rate limiting
batchProcess :: Int -> (a -> IO b) -> [a] -> IO [b]
batchProcess batchSize action items = do
  let batches = chunksOf batchSize items
  concat <$> mapM processBatch batches
  where
    processBatch batch = do
      results <- mapConcurrently action batch
      -- Small delay between batches to be polite to Hackage
      threadDelay 50_000  -- 50ms between batches
      pure results

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

logMsg :: String -> IO ()
logMsg msg = do
  hPutStrLn stderr msg
  hFlush stderr

showBuildResult :: BuildResult -> String
showBuildResult (BuildAttempted outcome)
  | buildSucceeded outcome && usedJailbreak outcome = "OK (jailbroken)"
  | buildSucceeded outcome                          = "OK"
  | usedJailbreak outcome                           = "FAILED (jailbroken)"
  | constraintsSolved outcome                       = "FAILED"
  | otherwise                                       = "CAN'T SOLVE"
showBuildResult BuildTimeout       = "TIMEOUT"
showBuildResult (BuildGetFailed _) = "GET FAILED"
