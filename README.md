# sched

薬袋式暗記シートの生成ツールです.

## 準備

以下のツールが必要です.
環境に応じて install してください.

- LiveTex (lualatex を叩いている)

## 使用方法

```bash
$ cabal run sched -- -h
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
  -o FILE  --output=FILE      Output file name (default: table.tex)
  -h       --help             Show this help message
```

薬袋式英単語暗記法におけるシートを生成したい場合は -u -n -r は指定する必要はありません.
応用して使いたい場合にはユニット数や繰り返し回数何日間に渡って計画を組むかを好きに指定してください.
