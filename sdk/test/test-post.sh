#!/bin/sh

. ./compare-sse.sh

[ ! -f "$1/input.json" ] && echo "case $1 does not have input.json" && return 1
[ ! -f "$1/output.txt" ] && echo "case $1 does not have output.txt" && return 1

[ -f "$1/testOutput.txt" ] && rm "$1/testOutput.txt"

input=$(cat "$1/input.json")

curl -sN -H "Accept: text/event-stream" -H "datastar-request: true" -H "Content-Type: application/json" --data "$input" "$2/test" -o "$1/testOutput.txt"

[ ! -f "$1/testOutput.txt" ] && echo "case $1 failed: your server did not return anything" && return 1

compare_sse_with_output "$1/output.txt" "$1/testOutput.txt" || { exit 1; }

exit 0
