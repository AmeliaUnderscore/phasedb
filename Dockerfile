# syntax=docker/dockerfile:1
#
# phasedb, PhaseDB HTTP server (Haskell / Warp).
#
# Builds the `phasedb-server` executable from source (GHC 9.10 + cabal) and
# ships a minimal runtime image. Default configuration: bind 127.0.0.1:8484,
# database file `phase.db` in the working directory.
#
#   docker build -t phasedb:latest -f phasedb/Dockerfile .
#   docker run --rm -it \
#     -p 8484:8484 \
#     -v $PWD/phase.db:/data/phase.db \         
#     phasedb:latest --db /data/phase.db --listen 0.0.0.0 --port 8484
#
# The first build pulls GHC + cabal plus roughly 15 Hackage packages
# (base, text, parsec, scientific, containers, time, ..., haskeline, warp,
# aeson, ...) and can take 10-20 minutes. Subsequent builds are incremental.

FROM haskell:9.10-bookworm AS builder

WORKDIR /build

# The haskell image ships GHC 9.10 + cabal. Extra libs cover the unix /
# network / cryptonite boundary when compiling on bookworm.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp-dev libffi-dev libncurses-dev libtinfo-dev libc6-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy sources first so dependency rebuilding is skipped when only source
# files change. This builds BOTH executables (phasedb REPL + phasedb-server).
COPY phasedb/phasedb-hs.cabal phasedb/LICENSE phasedb/CHANGELOG.md ./
COPY phasedb/src ./src
COPY phasedb/app ./app

RUN cabal update \
 && cabal build -O2 phasedb phasedb-server -j2 \
 && mkdir -p /out \
 && install -m 0755 "$(cabal list-bin -O2 phasedb-server)" /out/phasedb-server

FROM debian:bookworm-slim AS phasedb

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates passwd libgmp10 libffi8 libncurses6 libtinfo6 curl \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 1000 phasedb \
    && useradd --uid 1000 --gid phasedb --home-dir /home/phasedb --no-create-home phasedb

WORKDIR /home/phasedb
RUN mkdir -p /data && chown phasedb:phasedb /data

COPY --from=builder /out/phasedb-server .
COPY phasedb/docker-entrypoint.sh /usr/local/bin/phasedb-entrypoint
RUN chmod 0755 /usr/local/bin/phasedb-entrypoint

# The server binds 127.0.0.1:8484 by default and reads its database from
# the working directory (phase.db). Override via CLI flags (see below);
# e.g. --db /data/phase.db --listen 0.0.0.0 --port 8484.

EXPOSE 8484

USER phasedb

# CLI flag reference (§9.1 of the HTTP spec):
#   --db PATH            database file (default phase.db)
#   --listen ADDR        bind address (default 127.0.0.1)
#   --port N             TCP port (default 8484)
#   --cors-origin ORIGIN allow-browser CORS origin (off by default)
#   --tls-cert FILE      enable TLS (with --tls-key)
ENTRYPOINT ["/usr/local/bin/phasedb-entrypoint"]
CMD ["--help"]

# PhaseDB answers GET /v1/health with HTTP 200 (jsonOk).
HEALTHCHECK --interval=10s --timeout=5s --start-period=30s --retries=6 \
    CMD curl -sf http://127.0.0.1:8484/v1/health >/dev/null || exit 1
