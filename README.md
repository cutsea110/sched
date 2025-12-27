# sched

[![Haskell](https://github.com/cutsea110/sched/actions/workflows/haskell.yml/badge.svg)](https://github.com/cutsea110/sched/actions/workflows/haskell.yml)
[![Docker Cloud Build Status](https://img.shields.io/docker/pulls/cutsea110/sched?label=sched&logo=docker)](https://hub.docker.com/repository/docker/cutsea110/sched/general)

薬袋式暗記シートの生成ツールです.

## 準備

以下のツールが必要です.
環境に応じて install してください.

- LiveTex (lualatex を叩いている)

## 使用方法

```bash
              __             __
   __________/ /_  ___  ____/ /
  / ___/ ___/ __ \/ _ \/ __  / 
 (__  ) /__/ / / /  __/ /_/ /  
/____/\___/_/ /_/\___/\__,_/   
                               
Schedule Generator
> cabal run sched -- [OPTIONS...]
OPTION
  -u NUM   --unit-max=NUM     Maximum unit number (default: 232)
  -f DATE  --start-date=DATE  Schedule start date in YYYY-MM-DD format (default: today)
  -d NUM   --days=NUM         Number of days to schedule (default: 365)
  -r NUM   --repetitions=NUM  Number of repetitions (default: 18)
  -n       --no-date          Do not display dates in the table
  -o FILE  --output=FILE      Output file name (default: table.tex)
  -h       --help             Show this help message
```

薬袋式英単語暗記法におけるシートを生成したい場合は -u -d -r は指定する必要はありません.
書籍の指示通りに日付欄を空白にしたい場合には -n オプションを指定してください.
その他、他の暗記に薬袋式を応用したい場合にはユニット数や繰り返し回数、何日間に渡って計画を組むかを好きに指定してください.

## Docker イメージ作成

```bash
docker buildx build --rm --load -t cutsea110/sched:0.1.0 .
```

## Docker で実行

```bash
docker run -v ${PWD}:/work -it --rm cutsea110/sched:0.1.0 -n -o /work/sheet
```
