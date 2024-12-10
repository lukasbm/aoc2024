# Benchmarking tool

## Usage

When in this directory, build:
```bash
docker build -t aoc-benchmark:latest .
```

Then run:
```bash
docker run -v --rm "<input>:/app/input.txt:ro" -v "<src>:/app/main.hs:ro" aoc-benchmark:latest
```

Debugging conatiner:
```bash
docker run -it --rm --entrypoint=/bin/bash aoc-benchmark:latest
```
