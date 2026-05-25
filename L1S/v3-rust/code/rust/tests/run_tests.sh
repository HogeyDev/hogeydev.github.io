#!/bin/bash
set -e

TEST_DIR="../../../v3_ocaml/code/ocaml/tests/inputs"

cd "$(dirname "$0")/.."

echo "Building Rust compiler..."
cargo build --release 2>/dev/null

echo ""
echo "Running tests..."
all_pass=true
for input in "$TEST_DIR"/*.is; do
    name=$(basename "$input" .is)
    echo -n "  $name... "
    if cargo run --release -- "$input" -o "/tmp/${name}.s" 2>/dev/null; then
        if gcc -no-pie -o "/tmp/${name}" "/tmp/${name}.s" 2>/dev/null; then
            "/tmp/${name}"
            result=$?
            echo "$result"
        else
            echo "ASSEMBLY FAILED"
            all_pass=false
        fi
    else
        echo "COMPILE FAILED"
        all_pass=false
    fi
done

echo ""
if $all_pass; then
    echo "All tests passed!"
else
    echo "Some tests FAILED!"
    exit 1
fi
