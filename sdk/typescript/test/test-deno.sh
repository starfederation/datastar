#!/bin/bash

# Deno SDK Test Script
# Starts the Deno test server, runs tests, and cleans up

set -e  # Exit on any error

# Change to the typescript SDK directory
cd "$(dirname "$0")/.."

echo "Starting Deno test server..."
deno run --allow-net src/web/deno.ts &
SERVER_PID=$!

# Function to cleanup server on exit
cleanup() {
    if [ ! -z "$SERVER_PID" ]; then
        echo "Stopping test server..."
        kill $SERVER_PID 2>/dev/null || true
        wait $SERVER_PID 2>/dev/null || true
    fi
}

# Setup cleanup trap
trap cleanup EXIT

echo "Waiting for server to start..."
sleep 5

# Wait for server to be ready
max_attempts=15
attempt=1
while [ $attempt -le $max_attempts ]; do
    if curl -s http://localhost:8000/ > /dev/null 2>&1; then
        echo "Server ready! Running tests..."
        break
    fi
    
    if [ $attempt -eq $max_attempts ]; then
        echo "Error: Server failed to start after $max_attempts attempts"
        exit 1
    fi
    
    echo "Server not ready, waiting... (attempt $attempt/$max_attempts)"
    sleep 2
    attempt=$((attempt + 1))
done

# Store current directory
ORIGINAL_DIR=$(pwd)

# Run the test suite and track failures
cd ../test

echo "Running test suite manually to properly catch failures..."
FAILED_TESTS=0
TOTAL_TESTS=0

# Process GET cases
if [ -d "./get-cases" ]; then
    echo "Processing GET cases..."
    for case in ./get-cases/*; do
        if [ -d "$case" ]; then
            TOTAL_TESTS=$((TOTAL_TESTS + 1))
            echo "Running test: $(basename "$case")"
            if ! ./test-get.sh "$case" "http://localhost:8000"; then
                echo "  ❌ FAILED: $(basename "$case")"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            else
                echo "  ✅ PASSED: $(basename "$case")"
            fi
        fi
    done
    echo "Finished processing GET cases"
fi

# Process POST cases  
if [ -d "./post-cases" ]; then
    echo "Processing POST cases..."
    for case in ./post-cases/*; do
        if [ -d "$case" ]; then
            TOTAL_TESTS=$((TOTAL_TESTS + 1))
            echo "Running test: $(basename "$case")"
            if ! ./test-post.sh "$case" "http://localhost:8000"; then
                echo "  ❌ FAILED: $(basename "$case")"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            else
                echo "  ✅ PASSED: $(basename "$case")"
            fi
        fi
    done
    echo "Finished processing POST cases"
fi

# Return to original directory
cd "$ORIGINAL_DIR"

# Report results
echo ""
echo "=============================="
echo "Test Results Summary:"
echo "  Total tests: $TOTAL_TESTS"
echo "  Passed: $((TOTAL_TESTS - FAILED_TESTS))"
echo "  Failed: $FAILED_TESTS"
echo "=============================="

if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "💥 $FAILED_TESTS test(s) failed!"
    exit 1
fi 