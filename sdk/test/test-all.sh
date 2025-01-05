#!/bin/sh

#!/bin/sh

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <argument>"
  exit 1
fi

echo "Running tests with argument: $1"

# Run tests for GET cases
if [ -d "./get-cases" ]; then
  echo "Processing GET cases..."
  for case in ./get-cases/*; do
    if [ -d "$case" ]; then
      ./test-get.sh "$case" "$1"
    fi
  done
else
  echo "Directory './get-cases' not found!"
fi

# Run tests for POST cases
if [ -d "./post-cases" ]; then
  echo "Processing POST cases..."
  for case in ./post-cases/*; do
    if [ -d "$case" ]; then
      ./test-post.sh "$case" "$1"
    fi
  done
else
  echo "Directory './post-cases' not found!"
fi