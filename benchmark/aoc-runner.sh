#!/bin/bash

# compile
ghc -O2 -o main main.hs

# run (if stdin is not read, it does not matter ...)
time -f "%e" ./main "input.txt" < "input.txt"

runs=5
total_time=0

for i in $(seq 1 $runs); do
    exec_time=$( { /usr/bin/time -f "%e" ./main "input.txt" < "input.txt" 2>&1 1>/dev/null; } )
    # FIXME: handle failure
    echo "Execution time: $exec_time seconds"
    total_time=$(echo "$total_time + $exec_time" | bc)
done

average_time=$(echo "scale=3; $total_time / $runs" | bc)
echo "Average execution time: $average_time seconds"
