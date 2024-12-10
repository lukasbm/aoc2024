# Benchmarking tool

## Usage

```bash
docker build -t aoc-benchmark:latest .
docker run -v "./input.txt:/app/input.txt:ro" -v "./part2.hs:/app/main.hs:ro" aoc-benchmark:latest
```
