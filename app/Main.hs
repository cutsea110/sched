{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format ()
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute)
                             , getOpt, usageInfo
                             )
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Process (callProcess)

data Options = Options
  { optUnitMax     :: Int            -- ^ 最大ユニット数
  , optStartDate   :: Maybe Day      -- ^ スケジュール開始日
  , optNumOfDays   :: Int            -- ^ スケジュール日数
  , optRepetitions :: Int            -- ^ 繰り返し回数
  , optOutputFile  :: Maybe FilePath -- ^ 出力ファイル名
  , optHelp        :: Bool           -- ^ ヘルプ表示
  }

defaultOptions :: Options
defaultOptions = Options
  { optUnitMax     = 232
  , optStartDate   = Nothing
  , optNumOfDays   = 365
  , optRepetitions = 18
  , optOutputFile  = Nothing
  , optHelp        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['u'] ["unit-max"]
    (ReqArg (\arg opt -> opt { optUnitMax = read arg }) "NUM")
    "Maximum unit number (default: 232)"

  , Option ['f'] ["start-date"]
    (ReqArg (\arg opt -> opt { optStartDate = parseDay arg }) "DATE")
    "Schedule start date in YYYY-MM-DD format (default: today)"

  , Option ['d'] ["days"]
    (ReqArg (\arg opt -> opt { optNumOfDays = read arg }) "NUM")
    "Number of days to schedule (default: 365)"

  , Option ['r'] ["repetitions"]
    (ReqArg (\arg opt -> opt { optRepetitions = read arg }) "NUM")
    "Number of repetitions (default: 18)"

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
    (o, n, []  ) -> return (foldl' (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: cabal run ifl -- [OPTION...] <program-file>"

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  
today :: IO Day
today = utctDay <$> getCurrentTime

type Unit = Int
type Cycle = Int
type Schedule = [(Day, [[Unit]])]


dayN'sWork :: Unit      -- ^ Maximum unit number
           -> Int       -- ^ Repetition number
           -> Int       -- ^ Day number
           -> [[Unit]]  -- ^ List of cycles with units to work on that day
dayN'sWork unitMax repetition n = map (filter p) $ map rule [1..repetition]
  where p :: Int -> Bool
        p m = 0 < m && m <= unitMax
        rule :: Int -> [Unit]
        rule i | 1 <= i && i <= 7  =
                  let a = diff1 !! (i-1)
                  in [n-a]
                | 8 <= i && i <= 10 =
                  let a = diff2 !! (i-8)
                  in if odd (n-a) then [n-a, n-a+1] else []
                | 11 <= i           =
                  let a = diff4 !! (i-11)
                  in if quad (n-a) then [n-a-3,n-a-2,n-a-1, n-a] else []
                | otherwise         = error "unexpected index in rule"
          where
            quad :: Int -> Bool
            quad x = x `mod` 4 == 0

            diff1 = [0,1,4,9,17,29,46]
            diff2 = [67,88,109]
            diff4 = 129:[155,182..]
               


daysFromDay :: Day    -- ^ Starting day
            -> Int    -- ^ Number of days to generate
            -> [Day]  -- ^ List of days
daysFromDay start n = take n $ iterate (addDays 1) start


sched :: Unit      -- ^ Maximum unit number
      -> Int       -- ^ Number of repetitions
      -> Day       -- ^ Starting day
      -> Int       -- ^ Number of days to schedule
      -> Schedule  -- ^ Generated schedule
sched unitMax repetition start d = zip days (map (dayN'sWork unitMax repetition) [1..])
  where days = daysFromDay start d

sched' :: Unit     -- ^ Maximum unit number
       -> Int      -- ^ Number of repetitions
       -> String   -- ^ Starting date as string
       -> Int      -- ^ Number of days to schedule
       -> Schedule -- ^ Generated schedule
sched' unitMax repetition dateStr d = case parseDay dateStr of
  Just start -> sched unitMax repetition start d
  Nothing    -> error $ "Invalid date format: " ++ dateStr

lookupWork :: Schedule       -- ^ Schedule to look up
           -> String         -- ^ Date as string
           -> Maybe [[Unit]] -- ^ Work scheduled for that date
lookupWork m dateStr = case parseDay dateStr of
  Just day -> lookup day m
  Nothing  -> error $ "Invalid date format: " ++ dateStr

-- | 列数を明示してレンダリング（例: 18）
renderTable :: Int -> [(Day, [[Int]])] -> Text
renderTable maxCols rows =
  let nCols = min maxCols (max 0 (maximum (0 : map (length . snd) rows)))
      header = renderHeader nCols
      body   = T.concat (map (renderRow nCols) rows)
  in T.concat
      [ "\\begin{tabularx}{\\linewidth}{|l", T.replicate nCols "|c", "|}\n"
      , header
      , body
      , "\\end{tabularx}\n"
      ]

-- | ヘッダ: 日付 + 1回目..n回目
renderHeader :: Int -> Text
renderHeader nCols =
  let cols = [T.pack (show i) | i <- [1..nCols]]
  in T.concat
      [ "\\hline\n"
      , "Date & ", T.intercalate " & " cols, " \\\\\n"
      , "\\hline\n"
      , "\\endfirsthead\n"
      , "\\hline\n"
      , "Date & ", T.intercalate " & " cols, " \\\\\n"
      , "\\hline\n"
      , "\\endhead\n"
      , "\\hline\n"
      , "\\endfoot\n"
      ]

-- | 1行: 日付 + 各回のセル
renderRow :: Int -> (Day, [[Int]]) -> Text
renderRow nCols (day, cols0) =
  let cols = take nCols (cols0 ++ repeat [])  -- 足りない分は空セルで埋める
      dayTxt = formatDay day                -- 日付表示（好みに応じて変更）
      cells  = map renderCell cols
  in T.concat
      [ dayTxt
      , " & "
      , T.intercalate " & " cells
      , " \\\\\n"
      , "\\hline\n"
      ]

-- | セル: [Int] を "1・2" のように連結。空なら空文字。
-- 区切りを "," にしたいなら sep を "," に変えるだけでOK。
renderCell :: [Int] -> Text
renderCell xs = case xs of
  []        -> ""
  [a]       -> tshow a
  [a,b]     -> wrapSmall (tshow a <> ", " <> tshow b)
  [a,b,c,d] -> wrapSmall $
               "\\makecell[c]{"
               <> tshow a <> ", " <> tshow b
               <>  "\\\\"
               <> tshow c <> ", " <> tshow d
               <> "}"
  _         -> wrapSmall $
               "\\makecell[c]{"
               <> T.intercalate "\\\\" (chunks2 (map tshow xs))
               <> "}"
  where tshow = T.pack . show
        wrapSmall s = "\\smallcell{" <> s <> "}"
        chunks2 [] = []
        chunks2 [u] = [u]
        chunks2 (u:v:rs) = (u <> ", " <> v) : chunks2 rs

-- | 日付表示: "YYYY/MM/DD"
formatDay :: Day -> Text
formatDay d = T.pack (formatTime defaultTimeLocale "'%y %_m/%_d" d)

-- | LaTeXドキュメント全体を生成
latexDoc :: Text -> Text
latexDoc body =
  "\\documentclass[a4paper]{bxjsarticle}\n"
  <> "\\geometry{left=6mm,right=6mm,top=8mm,bottom=8mm}\n"
  <> "\\usepackage{ltablex}\n"
  <> "\\keepXColumns\n"
  <> "\\usepackage{array}\n"
  <> "\\usepackage{makecell}\n"
  <> "\\renewcommand\\tabularxcolumn[1]{m{#1}}\n"
  <> "\\setlength{\\tabcolsep}{2pt}\n"
  <> "\\renewcommand{\\arraystretch}{0.95}\n"
  <> "\\newcommand{\\smallcell}[1]{{\\scriptsize #1}}\n"

  <> "\\begin{document}\n"
  <> body
  <> "\n\\end{document}\n"

-- | 例: 最大232ユニット、2025-04-01から1年間のスケジュールを生成してPDF出力
writePdf :: Schedule -> FilePath -> IO ()
writePdf schedule fp = do
  writeFile fp (T.unpack (latexDoc (renderTable cols schedule)))
  callProcess "lualatex" [fp]
  callProcess "lualatex" [fp] -- ヘッダ幅をボディに合わせるため参照が必要なので2回実行
  where cols = foldl' (\acc (_, xs) -> acc `max` length xs) 0 schedule -- 全て同じ数なはず

-- This banner generated by using `figlet -f slant sched | sed "s@\\\@\\\\\\\@g"`.
helpMessage :: String
helpMessage = unlines
  [ "              __             __"
  , "   __________/ /_  ___  ____/ /"
  , "  / ___/ ___/ __ \\/ _ \\/ __  / "
  , " (__  ) /__/ / / /  __/ /_/ /  "
  , "/____/\\___/_/ /_/\\___/\\__,_/   "
  , "                               "
  , "Schedule Generator"
    , "> cabal run sched -- [OPTIONS...]"
  , usageInfo "OPTION" options
  ]

main :: IO ()
main = do
  args <- getArgs
  (opts, _) <- compilerOpts args

  when (optHelp opts) $ do
    putStr helpMessage
    exitSuccess

  startDate <- case optStartDate opts of
    Just d  -> return d
    Nothing -> liftIO today
  let unitMax    = optUnitMax opts
  let n          = optNumOfDays opts
  let repetition = optRepetitions opts
  let out        = case optOutputFile opts of
        Just fp -> fp
        Nothing -> "sched-minai-style.tex"
  let days = daysFromDay startDate n

  let m = zip days (map (dayN'sWork unitMax repetition) [1..])

  writePdf m out
