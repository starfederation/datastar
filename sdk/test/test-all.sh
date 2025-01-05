#!/bin/sh
echo "$1"
find ./get-cases -maxdepth 1 -mindepth 1 -exec ./test-get.sh {} "$1" \;
find ./post-cases -maxdepth 1 -mindepth 1 -exec ./test-post.sh {} "$1" \;
