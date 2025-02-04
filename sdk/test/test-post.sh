#!/bin/sh

[ ! -f "$1/input.json" ] && echo "case $1 does not have input.json" && return 1
[ ! -f "$1/output.txt" ] && echo "case $1 does not have output.txt" && return 1

[ -f "$1/testOutput.txt" ] && rm "$1/testOutput.txt"

input=$(cat "$1/input.json")
curl -sN -H "datastar-request" --json "$input" "$2/test" -o "$1/testOutput.txt"

[ ! -f "$1/testOutput.txt" ] && echo "case $1 failed: your server did not return anything" && return 1

./normalize.sh "$1/output.txt" >"$1/norm_output.txt"
./normalize.sh "$1/testOutput.txt" >"$1/norm_testOutput.txt"

diff -q "$1/norm_output.txt" "$1/norm_testOutput.txt" || { exit 1; }

rm "$1/norm_output.txt" "$1/norm_testOutput.txt"

exit 0
