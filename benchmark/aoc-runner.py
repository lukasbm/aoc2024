import os
from subprocess import run
import timeit


# from: https://stackoverflow.com/questions/11130156/suppress-stdout-stderr-print-from-python-functions
def block_output():
    # open two tmp fds
    null_fds = [os.open(os.devnull, os.O_RDWR) for x in range(2)]
    # save the current file descriptors to a tuple
    save = os.dup(1), os.dup(2)
    # put /dev/null fds on 1 and 2
    os.dup2(null_fds[0], 1)
    os.dup2(null_fds[1], 2)
    return save, null_fds


def restore_output(save, null_fds):
    # restore file descriptors so I can print the results
    os.dup2(save[0], 1)
    os.dup2(save[1], 2)
    # close the temporary fds
    os.close(null_fds[0])
    os.close(null_fds[1])


save, null_fds = block_output()

# compile (anything above o2 is claimed "unstable")
os.system("ghc -O2 -o main main.hs")

# read in the input
with open("input.txt", "rb") as f:
    input_text = f.read()


# single iteration execution
def execute_code():
    run(["./main", "input.txt"], capture_output=True, input=input_text, timeout=300, check=True)


# time it
# FIXME: there is a slight overhead, due to the subprocess call
res = timeit.repeat(execute_code, repeat=10, number=1)

restore_output(save, null_fds)

print(sum(res) / len(res))
