#!/bin/python3
import os
from subprocess import run
import timeit

# compile (anything above o2 is claimed "unstable")
os.system("ghc -O2 -o main main.hs")

with open("input.txt", "r") as f:
    input_text = f.read()


def execute_code():
    run(["./main", "input.txt"], input=input_text, timeout=300, check=True)


# time it
res = timeit.timeit(execute_code, number=10)

print(res)
