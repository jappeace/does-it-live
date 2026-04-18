module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time
  ( UTCTime(..), fromGregorian, secondsToDiffTime, addUTCTime
  , nominalDay )

import DoesItLive.Score
  ( scoreRecency, scoreStackage, scoreDeprecated
  , scoreReverseDeps, scoreVersionCount, scorePackage )
import DoesItLive.Stackage (parseStackageConfig, parseStackageVersions, formatProjectConstraints)
import DoesItLive.Hackage (parseUploadTimeHtml, parseReverseDepsHtml)
import Data.Map.Strict qualified as Map
import DoesItLive.Types (PackageInfo(..), ScoreResult(..), BuildStatus(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "does-it-live"
  [ recencyTests
  , stackageScoreTests
  , deprecatedTests
  , reverseDepsTests
  , versionCountTests
  , fullScoreTests
  , stackageParserTests
  , stackageVersionParserTests
  , projectConstraintsTests
  , reverseDepsParserTests
  , uploadTimeParserTests
  ]

-- A fixed reference time for tests: 2026-01-01 00:00:00 UTC
refTime :: UTCTime
refTime = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

daysAgo :: Integer -> Maybe UTCTime
daysAgo n = Just (addUTCTime (negate (fromIntegral n) * nominalDay) refTime)

recencyTests :: TestTree
recencyTests = testGroup "Upload recency scoring"
  [ testCase "no upload time gives 0" $
      scoreRecency refTime Nothing @?= 0
  , testCase "uploaded 30 days ago gives 30" $
      scoreRecency refTime (daysAgo 30) @?= 30
  , testCase "uploaded 200 days ago gives 25" $
      scoreRecency refTime (daysAgo 200) @?= 25
  , testCase "uploaded 500 days ago gives 20" $
      scoreRecency refTime (daysAgo 500) @?= 20
  , testCase "uploaded 800 days ago gives 15" $
      scoreRecency refTime (daysAgo 800) @?= 15
  , testCase "uploaded 1500 days ago gives 10" $
      scoreRecency refTime (daysAgo 1500) @?= 10
  , testCase "uploaded 3000 days ago gives 5" $
      scoreRecency refTime (daysAgo 3000) @?= 5
  , testCase "uploaded 4000 days ago gives 0" $
      scoreRecency refTime (daysAgo 4000) @?= 0
  ]

stackageScoreTests :: TestTree
stackageScoreTests = testGroup "Stackage score"
  [ testCase "in stackage gives 25" $
      scoreStackage True @?= 25
  , testCase "not in stackage gives 0" $
      scoreStackage False @?= 0
  ]

deprecatedTests :: TestTree
deprecatedTests = testGroup "Deprecated score"
  [ testCase "not deprecated gives 10" $
      scoreDeprecated False @?= 10
  , testCase "deprecated gives 0" $
      scoreDeprecated True @?= 0
  ]

reverseDepsTests :: TestTree
reverseDepsTests = testGroup "Reverse deps scoring"
  [ testCase "0 reverse deps gives 0" $
      scoreReverseDeps 0 @?= 0
  , testCase "1 reverse dep gives 3" $
      scoreReverseDeps 1 @?= 3
  , testCase "10 reverse deps gives 10" $
      scoreReverseDeps 10 @?= 10
  , testCase "100 reverse deps gives 20" $
      scoreReverseDeps 100 @?= 20
  , testCase "10000 reverse deps caps at 20" $
      scoreReverseDeps 10000 @?= 20
  ]

versionCountTests :: TestTree
versionCountTests = testGroup "Version count scoring"
  [ testCase "0 versions gives 0" $
      scoreVersionCount 0 @?= 0
  , testCase "1 version gives 2" $
      scoreVersionCount 1 @?= 2
  , testCase "3 versions gives 5" $
      scoreVersionCount 3 @?= 5
  , testCase "7 versions gives 10" $
      scoreVersionCount 7 @?= 10
  , testCase "15 versions gives 15" $
      scoreVersionCount 15 @?= 15
  ]

fullScoreTests :: TestTree
fullScoreTests = testGroup "Full package scoring"
  [ testCase "well-maintained package scores high" $
      let info = PackageInfo
            { packageName     = "aeson"
            , versionCount    = 50
            , lastUpload      = daysAgo 30
            , isDeprecated    = False
            , reverseDepCount = 5000
            , inStackage      = True
            }
          result = scorePackage refTime info
      in do
        totalScore result @?= 100
        scorePackageName result @?= "aeson"
        buildStatus result @?= BuildNotChecked
  , testCase "abandoned package scores low" $
      let info = PackageInfo
            { packageName     = "old-forgotten-pkg"
            , versionCount    = 1
            , lastUpload      = daysAgo 4000
            , isDeprecated    = True
            , reverseDepCount = 0
            , inStackage      = False
            }
          result = scorePackage refTime info
      in do
        totalScore result @?= 2
        buildStatus result @?= BuildNotChecked
  ]

stackageParserTests :: TestTree
stackageParserTests = testGroup "Stackage cabal.config parser"
  [ testCase "parses typical cabal.config" $
      let input = Text.unlines
            [ "constraints: aeson ==2.1.2.1,"
            , "             async ==2.2.5,"
            , "             base ==4.19.1.0,"
            , "             -- This is a comment"
            , "             text ==2.1.1"
            ]
          result = parseStackageConfig input
      in do
        Set.member "aeson" result @?= True
        Set.member "async" result @?= True
        Set.member "base" result @?= True
        Set.member "text" result @?= True
        Set.size result @?= 4
  , testCase "ignores comments" $
      let input = Text.unlines
            [ "-- This file is auto-generated"
            , "constraints: foo ==1.0"
            ]
          result = parseStackageConfig input
      in do
        Set.member "foo" result @?= True
        Set.size result @?= 1
  ]

stackageVersionParserTests :: TestTree
stackageVersionParserTests = testGroup "Stackage version parser"
  [ testCase "extracts name-version pairs" $
      let input = Text.unlines
            [ "constraints: aeson ==2.1.2.1,"
            , "             async ==2.2.5,"
            , "             base ==4.19.1.0,"
            , "             text ==2.1.1"
            ]
          result = parseStackageVersions input
      in do
        Map.lookup "aeson" result @?= Just "2.1.2.1"
        Map.lookup "async" result @?= Just "2.2.5"
        Map.lookup "base" result @?= Just "4.19.1.0"
        Map.lookup "text" result @?= Just "2.1.1"
        Map.size result @?= 4
  , testCase "ignores comments and with-compiler" $
      let input = Text.unlines
            [ "-- Stackage snapshot"
            , "with-compiler: ghc-9.10.3"
            , "constraints: foo ==1.0,"
            , "             bar ==2.0"
            ]
          result = parseStackageVersions input
      in do
        Map.lookup "foo" result @?= Just "1.0"
        Map.lookup "bar" result @?= Just "2.0"
        Map.size result @?= 2
  ]

projectConstraintsTests :: TestTree
projectConstraintsTests = testGroup "Project constraints formatter"
  [ testCase "prepends packages line and strips comments" $
      let input = Text.unlines
            [ "-- Auto-generated"
            , "with-compiler: ghc-9.10.3"
            , "constraints: aeson ==2.1.2.1,"
            , "             text ==2.1.1"
            ]
          result = formatProjectConstraints input
      in do
        -- Should start with "packages: ."
        Text.isPrefixOf "packages: ." result @?= True
        -- Should contain constraints
        Text.isInfixOf "constraints:" result @?= True
        Text.isInfixOf "aeson ==2.1.2.1" result @?= True
        -- Should not contain comments or with-compiler
        Text.isInfixOf "--" result @?= False
        Text.isInfixOf "with-compiler:" result @?= False
  ]

reverseDepsParserTests :: TestTree
reverseDepsParserTests = testGroup "Reverse deps HTML parser"
  [ testCase "parses reverse deps table" $
      let html = Text.concat
            [ "<table><tr class=\"fancy\"><th>Package name</th><th>Total</th><th>Direct</th></tr>"
            , "<tr class=\"odd\"><td><a href=\"/package/aeson\">aeson</a></td>"
            , "<td>3500 (<a href=\"/package/aeson/reverse/verbose\">view</a>)</td>"
            , "<td>1200 (<a href=\"/package/aeson/reverse\">view</a>)</td></tr>"
            , "<tr class=\"even\"><td><a href=\"/package/text\">text</a></td>"
            , "<td>8000 (<a href=\"/package/text/reverse/verbose\">view</a>)</td>"
            , "<td>4500 (<a href=\"/package/text/reverse\">view</a>)</td></tr>"
            , "</table>"
            ]
          result = parseReverseDepsHtml html
      in do
        Map.lookup "aeson" result @?= Just 1200
        Map.lookup "text" result @?= Just 4500
        Map.size result @?= 2
  , testCase "returns empty for no table rows" $
      let result = parseReverseDepsHtml "<html><body>no table</body></html>"
      in Map.size result @?= 0
  ]

uploadTimeParserTests :: TestTree
uploadTimeParserTests = testGroup "Upload time HTML parser"
  [ testCase "extracts upload timestamp" $
      let html = Text.concat
            [ "<table><tr><th>Uploaded</th>"
            , "<td>by <a href=\"/user/foo\">foo</a> at "
            , "<span title=\"Mon Jan 01 00:00:00 UTC 2026\">"
            , "2026-01-01T00:00:00Z</span></td></tr></table>"
            ]
      in parseUploadTimeHtml html @?= Just refTime
  , testCase "returns Nothing for missing upload info" $
      parseUploadTimeHtml "<html><body>no upload here</body></html>" @?= Nothing
  ]
