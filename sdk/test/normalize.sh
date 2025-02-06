#!/bin/sh

awk '
  # events are separated by '\n\n\n'
  BEGIN { RS = "\n\n\n"; FS = "\n"; }
  {
    # fragments and script
    ordered = ""
    # everything else
    unordered = ""

    # separate out ordered data lines
    for (i = 1; i <= NF; i++) {
      if ($i ~ /^data:(fragments|script)/)
        ordered = ordered $i "\n"
      else
        unordered = unordered $i "\n"
    }

    print unordered | "sort"
    close("sort", "to")
    print ordered;
  }
'
