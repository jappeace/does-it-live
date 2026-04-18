module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Csv (encodeDefaultOrderedByName)
import Options.Applicative
  ( Parser, execParser, info, helper, fullDesc, progDesc, header
  , strOption, option, auto, long, short, metavar, value, showDefault, help
  )

import DoesItLive (runScorer, Options(..))
import DoesItLive.Types ()

data CliOptions = CliOptions
  { cliOutput      :: FilePath
  , cliConcurrency :: Int
  , cliMinScore    :: Int
  }

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
  <$> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> value "output.csv"
     <> showDefault
     <> help "Output CSV file path"
      )
  <*> option auto
      ( long "concurrency"
     <> short 'c'
     <> metavar "N"
     <> value 20
     <> showDefault
     <> help "Maximum concurrent Hackage requests"
      )
  <*> option auto
      ( long "min-score"
     <> short 'm'
     <> metavar "N"
     <> value 0
     <> showDefault
     <> help "Only output packages scoring at or above this threshold"
      )

main :: IO ()
main = do
  cli <- execParser $ info (cliOptionsParser <* helper)
    ( fullDesc
   <> progDesc "Score Hackage packages on maintenance health (0-100)"
   <> header "does-it-live - Hackage package maintenance scorer"
    )

  let opts = Options
        { optOutput      = cliOutput cli
        , optConcurrency = cliConcurrency cli
        , optMinScore    = cliMinScore cli
        }

  results <- runScorer opts
  let csvData = encodeDefaultOrderedByName results
  LBS.writeFile (optOutput opts) csvData
  putStrLn ("Wrote " <> show (length results) <> " packages to " <> optOutput opts)
