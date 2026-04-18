module DoesItLive.Build
  ( BuildResult(..)
  , attemptBuild
  ) where

import Control.Exception (bracket_)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (ExitCode(..))
import System.Process.Typed
  ( proc, setWorkingDir, readProcess )
import System.Timeout (timeout)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist, listDirectory)

import DoesItLive.Types (BuildOutcome(..))

-- | Result of attempting to build a package
data BuildResult
  = BuildAttempted BuildOutcome
  | BuildTimeout
  | BuildGetFailed Text
  deriving stock (Show, Eq)

-- | Attempt to build a package.
-- 1. cabal get PKG[-VERSION] into workDir
-- 2. Write cabal.project with constraints (or just "packages: .")
-- 3. cabal build --dry-run to check constraint solving
-- 4. If solving fails, retry with --allow-newer (jailbreak)
-- 5. If solving succeeds, cabal build to compile
-- 6. Clean up the unpacked source
attemptBuild
  :: FilePath    -- ^ Working directory for unpacking
  -> Text        -- ^ cabal.project file content
  -> Text        -- ^ Package name
  -> Maybe Text  -- ^ Package version (Nothing = latest from Hackage)
  -> Int         -- ^ Timeout in seconds
  -> IO BuildResult
attemptBuild workDir projectContent packageName maybeVersion timeoutSeconds = do
  let getArg = case maybeVersion of
        Just version -> Text.unpack packageName <> "-" <> Text.unpack version
        Nothing      -> Text.unpack packageName

  -- cabal get to unpack the source
  (getExit, getStdout, getStderr) <- readProcess $
    proc "cabal" ["get", getArg, "--destdir=" <> workDir]
  case getExit of
    ExitFailure _ -> pure (BuildGetFailed (decodeOutput getStderr))
    ExitSuccess -> do
      -- Find the unpacked directory (cabal get creates PKG-VERSION/)
      packageDir <- findUnpackedDir workDir getStdout
      -- Write cabal.project in the package directory
      writeFile (packageDir <> "/cabal.project") (Text.unpack projectContent)
      -- Build with timeout, cleaning up package dir afterward
      let cleanupDir = do
            exists <- doesDirectoryExist packageDir
            if exists
              then removeDirectoryRecursive packageDir
              else pure ()
      bracket_ (pure ()) cleanupDir $ do
        result <- timeout (timeoutSeconds * 1_000_000) $ buildWithFallback packageDir
        case result of
          Nothing -> pure BuildTimeout
          Just outcome -> pure (BuildAttempted outcome)

-- | Try building: first normal, then with --allow-newer if solving fails
buildWithFallback :: FilePath -> IO BuildOutcome
buildWithFallback packageDir = do
  -- Step 1: Try dry-run to check constraint solving
  (dryExit, _, _) <- readProcess $
    setWorkingDir packageDir $ proc "cabal" ["build", "--dry-run"]
  case dryExit of
    ExitSuccess -> do
      -- Constraints solved, now actually build
      (buildExit, _, _) <- readProcess $
        setWorkingDir packageDir $ proc "cabal" ["build"]
      pure BuildOutcome
        { constraintsSolved = True
        , buildSucceeded    = buildExit == ExitSuccess
        , usedJailbreak     = False
        }
    ExitFailure _ -> do
      -- Constraints failed, try with --allow-newer
      (jailDryExit, _, _) <- readProcess $
        setWorkingDir packageDir $ proc "cabal" ["build", "--dry-run", "--allow-newer"]
      case jailDryExit of
        ExitFailure _ ->
          -- Can't solve even with --allow-newer
          pure BuildOutcome
            { constraintsSolved = False
            , buildSucceeded    = False
            , usedJailbreak     = False
            }
        ExitSuccess -> do
          -- Jailbroken solving works, try building
          (buildExit, _, _) <- readProcess $
            setWorkingDir packageDir $ proc "cabal" ["build", "--allow-newer"]
          pure BuildOutcome
            { constraintsSolved = False
            , buildSucceeded    = buildExit == ExitSuccess
            , usedJailbreak     = True
            }

-- | Find the unpacked package directory from cabal get output.
-- Stdout says "Unpacking to /path/PKG-VERSION/"
-- Falls back to listing the workDir for a single subdirectory.
findUnpackedDir :: FilePath -> LBS.ByteString -> IO FilePath
findUnpackedDir workDir stdout = do
  let outputText = decodeOutput stdout
  case parseUnpackPath outputText of
    Just path -> pure (Text.unpack (Text.strip path))
    Nothing -> do
      -- Fallback: find the directory cabal created
      entries <- listDirectory workDir
      case entries of
        [entry] -> pure (workDir <> "/" <> entry)
        _ -> pure workDir  -- shouldn't happen

-- | Parse "Unpacking to /some/path/" from cabal get stdout
parseUnpackPath :: Text -> Maybe Text
parseUnpackPath output =
  case Text.breakOn "Unpacking to " output of
    (_, rest)
      | not (Text.null rest) ->
        let afterPrefix = Text.drop (Text.length "Unpacking to ") rest
            path = Text.takeWhile (\c -> c /= '\n' && c /= '\r') afterPrefix
        in Just (Text.dropWhileEnd (== '/') path)
    _ -> Nothing

-- | Decode process output from lazy ByteString to Text
decodeOutput :: LBS.ByteString -> Text
decodeOutput = Text.decodeUtf8 . LBS.toStrict
