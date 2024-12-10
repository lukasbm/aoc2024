FROM haskell:9.4.8

RUN apt-get update && apt-get install -y python3

RUN adduser aoc
RUN mkdir -p /app
RUN chown -R aoc:aoc /app

COPY aoc-runner.py /app/aoc-runner.py

USER aoc

WORKDIR /app

# NOTE: these packages are install in aoc user space, so they are not available to the root user
# https://stackoverflow.com/questions/72376341/haskell-ghc-package-environment
RUN cabal update && cabal install --lib array adjunctions comonad directory parsec mtl containers split regex-compat regex-tdfa

ENTRYPOINT ["python3", "aoc-runner.py"]
