#!/bin/sh

[ ! -f "$1/input.json" ] && echo "case $1 does not have input.json" && return 1
[ ! -f "$1/output.txt" ] && echo "case $1 does not have output.txt" && return 1

echo "get test for $1 on $2"
input=$(cat "$1/input.json")
curl -sN --get --data-urlencode "datastar=$input"  "$2/test" -o "$1/testOutput.txt"

diff -q "$1/testOutput.txt" "$1/output.txt"

return 0
