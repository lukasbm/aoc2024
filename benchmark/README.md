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

Debugging conatiner (you can also keep the -v bindings):
```bash
docker run -it --rm --entrypoint=/bin/bash aoc-benchmark:latest
```

To run all scripts across all days use the `build-full.py` tool
