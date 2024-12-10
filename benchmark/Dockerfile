FROM haskell:9.4.8-slim

# https://stackoverflow.com/questions/72376341/haskell-ghc-package-environment
RUN cabal update && cabal install --force-reinstalls --lib array adjunctions comonad directory base parsec mtl containers regex-compat regex-tdfa split

RUN apt-get update && apt-get install -y python3

RUN mkdir -p /app

COPY aoc-runner.py /app/aoc-runner.py

RUN adduser aoc

USER aoc

WORKDIR /app

ENTRYPOINT ["python3", "aoc-runner.py"]
