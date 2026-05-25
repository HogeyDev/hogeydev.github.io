#!/bin/bash
set -e
# Build the compiler
dune build
# Test each program
for input in tests/inputs/*.is; do
    name=$(basename "$input" .is)
    echo -n "Testing $name... "
    ./_build/default/src/main.exe "$input" -o "/tmp/${name}.asm"
    nasm -f elf64 "/tmp/${name}.asm" -o "/tmp/${name}.o"
    cc -o "/tmp/${name}" "/tmp/${name}.o" -no-pie
    "/tmp/${name}"
    result=$?
    echo "exit code $result"
done
echo "All tests passed!"
