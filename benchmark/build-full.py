#!/bin/python3
import os
import subprocess
from pathlib import Path
from typing import Dict, Optional, Tuple
import csv


# FIXME: adapt!
DAVID_REPO = Path("/home/lukas/Documents/test/aoc2024-david")
LUKAS_REPO = Path("/home/lukas/Documents/test/aoc2024-lukas")
FERDINAND_REPO = Path("/home/lukas/Documents/test/aoc2024-ferdinand")


# https://stackoverflow.com/questions/41171791/how-to-suppress-or-capture-the-output-of-subprocess-run
def docker_time(input_file: Path, script_file: Path) -> float:
    try:
        result = subprocess.run(
            f"docker run --rm -v {str(script_file)}:/app/main.hs:ro -v {str(input_file)}:/app/input.txt:ro aoc-benchmark:latest".split(" "), capture_output=True, check=True
        )
        return float(result.stdout)
    except subprocess.CalledProcessError as e:
        print(f"Command {' '.join(e.cmd)} failed with {e.returncode} - {e.stderr.decode()} - {e.stdout.decode()}")
        return None


def run_david(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = DAVID_REPO / str(day) / "1.hs"
    part2 = DAVID_REPO / str(day) / "2.hs"
    input = get_input(day)

    part1_time = docker_time(input, part1) if part1.exists() else None
    part2_time = docker_time(input, part2) if part2.exists() else None

    return part1_time, part2_time


def run_ferdinand(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = FERDINAND_REPO / "app" / f"day{day:02}.hs"
    part2 = FERDINAND_REPO / "app" / f"day{day:02}p2.hs"
    input = get_input(day)

    part1_time = docker_time(input, part1) if part1.exists() else None
    part2_time = docker_time(input, part2) if part2.exists() else None

    return part1_time, part2_time


def run_lukas(day: int) -> Tuple[Optional[float], Optional[float]]:
    part1 = LUKAS_REPO / f"{day:02}" / "part1.hs"
    part2 = LUKAS_REPO / f"{day:02}" / "part2.hs"
    input = get_input(day)

    part1_time = docker_time(input, part1) if part1.exists() else None
    part2_time = docker_time(input, part2) if part2.exists() else None

    return part1_time, part2_time


def get_input(day: int) -> Path:
    return LUKAS_REPO / f"{day:02}" / "input.txt"


res: Dict[int, Dict[str, Tuple[int, int]]] = {}

for day in range(1, 26):
    res[day] = {}
    print(f"day {day} - lukas")
    res[day]["lukas"] = run_lukas(day)
    print(f"day {day} - david")
    res[day]["david"] = run_david(day)
    print(f"day {day} - ferdinand")
    res[day]["ferdinand"] = run_ferdinand(day)

with open("bench.csv", "w+", newline="") as csvfile:
    fieldnames = ["day", "name", "part1", "part2"]
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    for day, data in res.items():
        for name, times in data.items():
            writer.writerow({"day": day, "name": name, "part1": times[0], "part2": times[1]})
