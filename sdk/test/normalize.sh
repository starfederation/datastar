#!/bin/sh

awk '
  BEGIN { RS = "\n\n\n"; FS = "\n"; }

  function flush_data(data) {
    if (data) {
      print data | "sort"
      close("sort")
    }
    return ""
  }

  {
    data = ""

    for (i = 1; i <= NF; i++) {
      if ($i ~ /^data: (fragments|script|path)/) {
        data = flush_data(data)
        print $i
      } else if ($i ~ /^data:/) {
        data = data $i "\n"
      } else {
        data = flush_data(data)
        print $i
      }
    }

    # flush remaining unordered data lines
    flush_data(data)
  }
' "$1"
