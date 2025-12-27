{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Format ()
import System.Process (callProcess)

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  
today :: IO Day
today = utctDay <$> getCurrentTime

type Unit = Int
type Cycle = Int
type Schedule = [(Day, [[Unit]])]


dayN'sWork :: Unit      -- ^ Maximum unit number
           -> Int       -- ^ Day number
           -> [[Unit]]  -- ^ List of cycles with units to work on that day
dayN'sWork unitMax n = map (filter p) $ map rule [1..18]
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
      -> Day       -- ^ Starting day
      -> Int       -- ^ Number of days to schedule
      -> Schedule  -- ^ Generated schedule
sched unitMax start d = zip days (map (dayN'sWork unitMax) [1..])
  where days = daysFromDay start d

sched' :: Unit     -- ^ Maximum unit number
       -> String   -- ^ Starting date as string
       -> Int      -- ^ Number of days to schedule
       -> Schedule -- ^ Generated schedule
sched' unitMax dateStr d = case parseDay dateStr of
  Just start -> sched unitMax start d
  Nothing    -> error $ "Invalid date format: " ++ dateStr

lookupWork :: Schedule       -- ^ Schedule to look up
           -> String         -- ^ Date as string
           -> Maybe [[Unit]] -- ^ Work scheduled for that date
lookupWork m dateStr = case parseDay dateStr of
  Just day -> lookup day m
  Nothing  -> error $ "Invalid date format: " ++ dateStr

-- | 既定: 列数はデータから推定し、最大18列に丸める（必要なら変更）
renderTable :: Schedule -> Text
renderTable = renderTableN 18

-- | 列数を明示してレンダリング（例: 18）
renderTableN :: Int -> [(Day, [[Int]])] -> Text
renderTableN maxCols rows =
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
writePdf :: Schedule -> IO ()
writePdf schedule = do
  writeFile "table.tex" (T.unpack (latexDoc (renderTable schedule)))
  callProcess "lualatex" ["table.tex"]
  callProcess "lualatex" ["table.tex"] -- ヘッダ幅をボディに合わせるため参照が必要なので2回実行
  
main :: IO ()
main = do
  let day = case parseDay "2025-04-01" of
              Just d  -> d
              Nothing -> error "Invalid date format"
  let days = daysFromDay day 365
  let m = zip days (map (dayN'sWork 232) [1..])
  writePdf m
  -- TIO.putStrLn (latexDoc (renderTable m))
