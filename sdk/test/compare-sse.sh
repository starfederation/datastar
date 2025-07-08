#!/bin/sh

# Usage: compare-sse.sh expected.txt actual.txt
# Compares two SSE files allowing fields to be in any order
# but preserving order within same prefix types

if [ $# -ne 2 ]; then
    echo "Usage: $0 expected.txt actual.txt" >&2
    exit 1
fi

expected="$1"
actual="$2"

if [ ! -f "$expected" ]; then
    echo "Expected file not found: $expected" >&2
    exit 1
fi

if [ ! -f "$actual" ]; then
    echo "Actual file not found: $actual" >&2
    exit 1
fi

# Use awk to parse and compare SSE events
awk '
BEGIN {
    RS = "\n\n\n"  # Events separated by double newlines
    FS = "\n"      # Fields separated by single newlines
    event_count_1 = 0
    event_count_2 = 0
}

# Process first file (expected)
NR == FNR {
    if (NF > 0) {
        event_count_1++
        delete fields
        delete field_order
        field_count = 0
        
        # Parse fields and group by prefix
        for (i = 1; i <= NF; i++) {
            if ($i != "") {
                field_count++
                # Extract prefix (everything before first colon)
                prefix = $i
                sub(/:.*/, "", prefix)
                
                # Store field content
                if (!(prefix in fields)) {
                    fields[prefix] = ""
                    field_order[prefix] = 0
                }
                if (fields[prefix] != "") {
                    fields[prefix] = fields[prefix] "\n"
                }
                fields[prefix] = fields[prefix] $i
                field_order[prefix]++
            }
        }
        
        # Store event data
        for (prefix in fields) {
            events_1[event_count_1, prefix] = fields[prefix]
            event_field_count_1[event_count_1, prefix] = field_order[prefix]
        }
    }
    next
}

# Process second file (actual)
{
    if (NF > 0) {
        event_count_2++
        delete fields
        delete field_order
        field_count = 0
        
        # Parse fields and group by prefix
        for (i = 1; i <= NF; i++) {
            if ($i != "") {
                field_count++
                # Extract prefix
                prefix = $i
                sub(/:.*/, "", prefix)
                
                # Store field content
                if (!(prefix in fields)) {
                    fields[prefix] = ""
                    field_order[prefix] = 0
                }
                if (fields[prefix] != "") {
                    fields[prefix] = fields[prefix] "\n"
                }
                fields[prefix] = fields[prefix] $i
                field_order[prefix]++
            }
        }
        
        # Store event data
        for (prefix in fields) {
            events_2[event_count_2, prefix] = fields[prefix]
            event_field_count_2[event_count_2, prefix] = field_order[prefix]
        }
    }
}

END {
    # Compare event counts
    if (event_count_1 != event_count_2) {
        print "Event count mismatch: expected " event_count_1 ", got " event_count_2 > "/dev/stderr"
        exit 1
    }
    
    # Compare each event
    for (e = 1; e <= event_count_1; e++) {
        # Collect all prefixes from both events
        delete all_prefixes
        for (key in events_1) {
            split(key, parts, SUBSEP)
            if (parts[1] == e) {
                all_prefixes[parts[2]] = 1
            }
        }
        for (key in events_2) {
            split(key, parts, SUBSEP)
            if (parts[1] == e) {
                all_prefixes[parts[2]] = 1
            }
        }
        
        # Compare fields for each prefix
        for (prefix in all_prefixes) {
            key1 = e SUBSEP prefix
            key2 = e SUBSEP prefix
            
            # Check if prefix exists in both
            if (!(key1 in events_1) && !(key2 in events_2)) {
                continue
            }
            
            if (!(key1 in events_1)) {
                print "Event " e ": missing prefix \"" prefix "\" in expected" > "/dev/stderr"
                exit 1
            }
            
            if (!(key2 in events_2)) {
                print "Event " e ": missing prefix \"" prefix "\" in actual" > "/dev/stderr"
                exit 1
            }
            
            # Compare field content
            if (events_1[key1] != events_2[key2]) {
                print "Event " e ": mismatch in \"" prefix "\" fields" > "/dev/stderr"
                print "Expected:" > "/dev/stderr"
                print events_1[key1] > "/dev/stderr"
                print "Actual:" > "/dev/stderr"
                print events_2[key2] > "/dev/stderr"
                exit 1
            }
        }
    }
    
    # All matches
    exit 0
}
' "$expected" "$actual"