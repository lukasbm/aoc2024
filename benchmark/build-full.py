#!/bin/python3
import os
import subprocess
from pathlib import Path
from typing import Optional, Tuple

# FIXME: adapt!
DAVID_REPO = Path("david")
LUKAS_REPO = Path("/home/lukas/Documents/projects/aoc2024")
FERDINAND_REPO = Path("ferdinand")


# https://stackoverflow.com/questions/41171791/how-to-suppress-or-capture-the-output-of-subprocess-run
def docker_time(input_file: Path, script_file: Path) -> float:
    result = subprocess.run(
        f'docker run --rm -v "{str(script_file)}:/app/main.hs:ro" -v "{str(input_file)}:/app/input.txt:ro" aoc-benchmark:latest', capture_output=True, check=True
    )
    return float(result.stdout)


def run_david(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = DAVID_REPO / str(day) / "1.hs"
    part2 = DAVID_REPO / str(day) / "2.hs"

    part1_time = docker_time(get_input(day), part1) if part1.exists() else None
    part2_time = docker_time(get_input(day), part2) if part2.exists() else None

    return part1_time, part2_time


def run_ferdinand(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = FERDINAND_REPO / "app" / f"day{day:02}.hs"
    part1 = FERDINAND_REPO / "app" / f"day{day:02}p2.hs"

    part1_time = docker_time(get_input(day), part1) if part1.exists() else None
    part2_time = docker_time(get_input(day), part2) if part2.exists() else None

    return part1_time, part2_time


def run_lukas(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = LUKAS_REPO / f"{day:02}" / "part1.hs"
    part1 = LUKAS_REPO / f"{day:02}" / "part2.hs"

    part1_time = docker_time(get_input(day), part1) if part1.exists() else None
    part2_time = docker_time(get_input(day), part2) if part2.exists() else None

    return part1_time, part2_time


def get_input(day: int) -> Path:
    return LUKAS_REPO / f"{day:02}" / "input.txt"


for day in range(1, 26):
    print(run_lukas(day))
    # run_david(day)
    # run_ferdinand(day)
    # run_lukas(day)
    # print(f"Day {day} done")
