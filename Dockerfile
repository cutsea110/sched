# ---- builder (GHCあり) ----
FROM debian:bookworm-slim AS builder
ARG GHC_VERSION=9.12.2
ARG CABAL_VERSION=3.14.2.0
ENV DEBIAN_FRONTEND=noninteractive
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION}
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION}
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=0

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl git make \
    gcc g++ libc6-dev libffi-dev libgmp-dev zlib1g-dev pkg-config \
 && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/root/.ghcup/bin:/root/.cabal/bin:${PATH}

WORKDIR /src
COPY . .
RUN cabal update && cabal build -O2
# 例：実行ファイルの取り出し（プロジェクトに合わせて調整）
RUN mkdir -p /out && \
    cp "$(cabal list-bin exe:sched)" /out/sched

# ---- runtime (GHCなし) ----
FROM debian:bookworm-slim
ARG TEXLIVE_PROFILE=medium
ENV DEBIAN_FRONTEND=noninteractive

# TeX Live + runtime deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 libffi8 zlib1g \
 && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
  apt-get update; \
  if [ "$TEXLIVE_PROFILE" = "full" ]; then \
    apt-get install -y --no-install-recommends texlive-full; \
  elif [ "$TEXLIVE_PROFILE" = "minimal" ]; then \
    apt-get install -y --no-install-recommends texlive-base texlive-latex-base latexmk; \
  else \
    apt-get install -y --no-install-recommends \
      texlive-base \
      texlive-latex-base texlive-latex-recommended texlive-latex-extra \
      texlive-fonts-recommended \
      texlive-lang-japanese \
      latexmk biber ghostscript fonts-noto-cjk; \
  fi; \
  rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY --from=builder /out/sched /usr/local/bin/sched
RUN chmod +x /usr/local/bin/sched

ENTRYPOINT ["sched"]
CMD []
