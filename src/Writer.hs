{-# LANGUAGE OverloadedStrings #-}
module Writer (writePdf) where

import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (callProcess)

import Types (Schedule, DayItem)

-- | 日付表示: "YYYY/MM/DD"
formatDay :: Day -> Text
formatDay d = T.pack (formatTime defaultTimeLocale "'%y %_m/%_d" d)

-- | 列数を明示してレンダリング（例: 18）
renderTable :: Bool          -- ^ 日付非表示フラグ
            -> Int           -- ^ 最大列数
            -> Schedule      -- ^ スケジュールデータ
            -> Text
renderTable noDate maxCols rows =
  T.unlines [ "\\begin{tabularx}{\\linewidth}{|l", T.replicate nCols "|c", "|}"
            , header
            , body
            , "\\end{tabularx}"
            ]
  where nCols  = min maxCols (max 0 (maximum (0 : map (length . snd) rows)))
        header = renderHeader nCols
        body   = T.unlines (map (renderRow noDate nCols) rows)

-- | ヘッダ: 日付 + 1回目..n回目
renderHeader :: Int -> Text
renderHeader nCols =
  T.unlines [ "\\hline"
            , "Date & ", T.intercalate " & " cols, " \\\\"
            , "\\hline"
            , "\\endfirsthead"
            , "\\hline"
            , "Date & ", T.intercalate " & " cols, " \\\\"
            , "\\hline"
            , "\\endhead"
            , "\\hline"
            , "\\endfoot"
            ]
  where cols = [T.pack (show i) | i <- [1..nCols]]

-- | 1行: 日付 + 各回のセル
renderRow :: Bool            -- ^ 日付非表示フラグ
          -> Int             -- ^ 列数
          -> DayItem         -- ^ 1行分のデータ
          -> Text
renderRow noDate nCols (day, cols0) =
  T.unlines [ dayTxt <> " & " <> T.intercalate " & " cells <> " \\\\"
            , "\\hline"
            ]
  where cols   = take nCols (cols0 ++ repeat [])  -- 足りない分は空セルで埋める
        dayTxt | noDate    = T.pack "\\texttt{  /  /  }"
               | otherwise = formatDay day                -- 日付表示（好みに応じて変更）
        cells  = map renderCell cols

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

        chunks2 []       = []
        chunks2 [u]      = [u]
        chunks2 (u:v:rs) = (u <> ", " <> v) : chunks2 rs

-- | LaTeXドキュメント全体を生成
latexDoc :: Text -> Text -> Text
latexDoc ttl body = T.unlines
  [ "\\documentclass[a4paper]{bxjsarticle}"
  , "\\usepackage{fancyhdr}"
  , "\\geometry{left=6mm,right=6mm,top=8mm,bottom=8mm}"
  , "\\usepackage{ltablex}"
  , "\\keepXColumns"
  , "\\usepackage{array}"
  , "\\usepackage{makecell}"
  , "\\renewcommand\\tabularxcolumn[1]{m{#1}}"
  , "\\setlength{\\tabcolsep}{2pt}"
  , "\\renewcommand{\\arraystretch}{0.95}"
  , "\\newcommand{\\smallcell}[1]{{\\scriptsize #1}}"
  , "\\usepackage{fontspec}"
  , "\\usepackage{luatexja}"
  , "\\usepackage{luatexja-fontspec}"
  , "\\setmainfont{Comic Neue}[UprightFeatures={FakeSlant=0.2}]"
  , "\\setmainjfont{M PLUS 1 Code}"

  , "\\pagestyle{fancy}"
  , "\\fancyhf{}"
  , "\\fancyhead[R]{" <> ttl <> "}"
  , "\\fancyfoot[C]{\\thepage}"

  , "\\begin{document}"
  , body
  , "\\end{document}"
  ]

-- | 例: 最大232ユニット、2025-04-01から1年間のスケジュールを生成してPDF出力
writePdf :: Bool       -- ^ 日付非表示フラグ
         -> Schedule   -- ^ スケジュールデータ
         -> Text       -- ^ タイトル
         -> FilePath   -- ^ 出力ファイルパス
         -> IO ()
writePdf noDate schedule ttl fp = do
  writeFile fp (T.unpack (latexDoc ttl (renderTable noDate cols schedule)))
  callProcess "latexmk" [fp]
  where cols = foldl' (\acc (_, xs) -> acc `max` length xs) 0 schedule -- 全て同じ数なはず

