import os
import subprocess
import timeit


# compile (anything above o2 is claimed "unstable")
subprocess.run(["ghc", "-O2", "-o", "main", "main.hs"], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# read in the input
with open("input.txt", "rb") as f:
    input_text = f.read()


# single iteration execution
def execute_code():
    subprocess.run(["./main", "input.txt"], input=input_text, timeout=300, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


# time it
# FIXME: there is a slight overhead, due to the subprocess call
res = timeit.repeat(execute_code, repeat=5, number=1)

print(sum(res) / len(res))
