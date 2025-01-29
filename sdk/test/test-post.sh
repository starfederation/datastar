#!/bin/sh

[ ! -f "$1/input.json" ] && echo "case $1 does not have input.json" && return 1
[ ! -f "$1/output.txt" ] && echo "case $1 does not have output.txt" && return 1

rm "$1/testOutput.txt"

input=$(cat "$1/input.json")
curl -sN -H "datastar-request"  --json  "$input"  "$2/test" -o "$1/testOutput.txt"

[ ! -f "$1/testOutput.txt" ] && echo "case $1 failed: your server did not return anything" && return 1

diff -q "$1/testOutput.txt" "$1/output.txt"

return 0
