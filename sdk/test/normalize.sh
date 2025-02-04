#!/bin/sh

# remove single space after colons if any
sed 's/: \([^ ]\|$\)/:\1/g' "$1" |
  awk '
    # events are separated by '\n\n\n'
    BEGIN { RS = "\n\n\n"; FS = "\n"; }
    {
      # sort each line in the event
      print $0 | "sort"
      close("sort", "to")
    }
  '
