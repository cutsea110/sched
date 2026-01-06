module Opts (Options(..), getOpts, options, today, usageInfo) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Time (Day, defaultTimeLocale, getCurrentTime, getCurrentTimeZone, localDay, parseTimeM, utcToLocalTime)
import Data.Time.Format ()
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt, usageInfo)
import System.Environment (getArgs)

data Options = Options
  { optNumOfUnits  :: Int            -- ^ ユニット数
  , optStartDate   :: Maybe Day      -- ^ スケジュール開始日
  , optNumOfDays   :: Int            -- ^ スケジュール日数
  , optRepetitions :: Int            -- ^ 繰り返し回数
  , optNoDate      :: Bool           -- ^ 日付非表示
  , optTitle       :: Maybe String   -- ^ タイトル
  , optOutputFile  :: Maybe FilePath -- ^ 出力ファイル名
  , optHelp        :: Bool           -- ^ ヘルプ表示
  }

defaultOptions :: Options
defaultOptions = Options
  { optNumOfUnits  = 232
  , optStartDate   = Nothing
  , optNumOfDays   = 365
  , optRepetitions = 18
  , optNoDate      = False
  , optTitle       = Nothing
  , optOutputFile  = Nothing
  , optHelp        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['u'] ["unit-max"]
    (ReqArg (\arg opt -> opt { optNumOfUnits = read arg }) "NUM")
    "Number of units (default: 232)"

  , Option ['f'] ["start-date"]
    (ReqArg (\arg opt -> opt { optStartDate = parseDay arg }) "DATE")
    "Schedule start date in YYYY-MM-DD format (default: today)"

  , Option ['d'] ["days"]
    (ReqArg (\arg opt -> opt { optNumOfDays = read arg }) "NUM")
    "Number of days to schedule (default: 365)"

  , Option ['r'] ["repetitions"]
    (ReqArg (\arg opt -> opt { optRepetitions = read arg }) "NUM")
    "Number of repetitions (default: 18)"

  , Option ['n'] ["no-date"]
    (NoArg (\opt -> opt { optNoDate = True }))
    "Do not display dates in the table"

  , Option ['t'] ["title"]
    (ReqArg (\arg opt -> opt { optTitle = Just arg }) "TITLE")
    "Title to display in the header"

  , Option ['o'] ["output"]
    (ReqArg (\arg opt -> opt { optOutputFile = Just arg }) "FILE")
    "Output file name (default: table.tex)"

  , Option ['h'] ["help"]
    (NoArg (\opt -> opt { optHelp = True }))
    "Show this help message"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl' (&) defaultOptions o, n)
    (_, _, errs) -> ioError $ userError (concat errs ++ usageInfo header options)
  where header = "Usage: cabal run ifl -- [OPTION...] <program-file>"

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  
today :: MonadIO m => m Day
today = liftIO $ do
  utcTime <- getCurrentTime
  tz      <- getCurrentTimeZone
  pure $ localDay $ utcToLocalTime tz utcTime

getOpts :: IO Options
getOpts = do
  args <- getArgs
  (opts, _) <- compilerOpts args
  return opts

