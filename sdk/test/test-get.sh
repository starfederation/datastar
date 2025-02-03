#!/bin/sh

[ ! -f "$1/input.json" ] && echo "case $1 does not have input.json" && return 1
[ ! -f "$1/output.txt" ] && echo "case $1 does not have output.txt" && return 1

input=$(cat "$1/input.json")

[ -f "$1/testOutput.txt" ] && rm "$1/testOutput.txt"

curl -sN --get -H "datastar-request" --data-urlencode "datastar=$input" "$2/test" -o "$1/testOutput.txt"

[ ! -f "$1/testOutput.txt" ] && echo "case $1 failed: your server did not return anything" && return 1

diff -q "$1/testOutput.txt" "$1/output.txt"

exit 0
