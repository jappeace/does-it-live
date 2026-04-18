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
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)

-- | Result of attempting to build a package
data BuildResult
  = BuildSuccess
  | BuildFailure Text
  | BuildTimeout
  | BuildSkipped
  deriving stock (Show, Eq)

-- | Attempt to build a package using Stackage LTS constraints.
-- 1. cabal get PKG-VERSION into workDir
-- 2. Write cabal.project with Stackage constraints
-- 3. cabal build with timeout
-- 4. Clean up the unpacked source (cabal store persists as cache)
attemptBuild
  :: FilePath  -- ^ Working directory for unpacking
  -> Text      -- ^ cabal.project file content (constraints)
  -> Text      -- ^ Package name
  -> Text      -- ^ Package version
  -> Int       -- ^ Timeout in seconds
  -> IO BuildResult
attemptBuild workDir projectContent packageName version timeoutSeconds = do
  let nameVersion = Text.unpack packageName <> "-" <> Text.unpack version
      packageDir = workDir <> "/" <> nameVersion

  -- cabal get to unpack the source
  (getExit, _, getStderr) <- readProcess $
    proc "cabal" ["get", nameVersion, "--destdir=" <> workDir]
  case getExit of
    ExitFailure _ -> pure (BuildFailure (decodeOutput getStderr))
    ExitSuccess -> do
      -- Write cabal.project in the package directory
      writeFile (packageDir <> "/cabal.project") (Text.unpack projectContent)
      -- Build with timeout, cleaning up package dir afterward
      let cleanupDir = do
            exists <- doesDirectoryExist packageDir
            if exists
              then removeDirectoryRecursive packageDir
              else pure ()
      bracket_ (pure ()) cleanupDir $ do
        let buildProc = setWorkingDir packageDir $ proc "cabal" ["build"]
        result <- timeout (timeoutSeconds * 1_000_000) $
          readProcess buildProc
        case result of
          Nothing -> pure BuildTimeout
          Just (ExitSuccess, _, _) -> pure BuildSuccess
          Just (ExitFailure _, _, buildStderr) ->
            pure (BuildFailure (decodeOutput buildStderr))

-- | Decode process output from lazy ByteString to Text
decodeOutput :: LBS.ByteString -> Text
decodeOutput = Text.decodeUtf8 . LBS.toStrict
