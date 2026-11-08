FROM haskell:9.10.3-slim-bookworm AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY stack.yaml stack.yaml.lock package.yaml yaptape.cabal ./
RUN stack build --only-dependencies --system-ghc

COPY app ./app
COPY src ./src
COPY static ./static
COPY db ./db
COPY LICENSE README.md Setup.hs ./

RUN stack install --system-ghc --local-bin-path /out

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libpq5 \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --shell /bin/bash yaptape

WORKDIR /app

COPY --from=build /out/yaptape-exe /usr/local/bin/yaptape-exe
COPY static ./static
COPY db/migrations ./db/migrations

ENV PORT=3000 \
    STATIC_DIR=/app/static \
    MIGRATIONS_DIR=/app/db/migrations

USER yaptape
EXPOSE 3000

CMD ["yaptape-exe"]
