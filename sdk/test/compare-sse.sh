#!/bin/sh

# Usage: compare-sse.sh expected.txt actual.txt
# Compares two SSE files allowing fields to be in any order
# but preserving order within same prefix types

# Function for comparing SSE outputs with error display
compare_sse_with_output() {
    expected="$1"
    actual="$2"
    
    # Call the main comparison logic
    compare_sse "$expected" "$actual" || { 
        echo "Difference between expected and actual output:"
        echo ""
        diff -u "$expected" "$actual" || true
        return 1
    }
    return 0
}

# Main comparison function
compare_sse() {
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
    RS = "\n\n"  # Events separated by double newlines
    FS = "\n"      # Fields separated by single newlines
    event_count_1 = 0
    event_count_2 = 0
}

# Process first file (expected)
NR == FNR {
    if (NF > 0) {
        event_count_1++
        delete data_fields
        delete other_fields
        data_count = 0
        
        # Parse fields and separate data fields from others
        for (i = 1; i <= NF; i++) {
            if ($i != "") {
                # Extract prefix (everything before first colon)
                prefix = $i
                sub(/:.*/, "", prefix)
                
                if (prefix == "data") {
                    # Extract subgroup (first word after "data: ")
                    subgroup = $i
                    sub(/^data: /, "", subgroup)
                    sub(/ .*$/, "", subgroup)
                    
                    # Store data fields with subgroup for sorting
                    data_count++
                    data_fields[data_count] = $i
                    data_subgroups[data_count] = subgroup
                } else {
                    # Store non-data fields by prefix
                    if (!(prefix in other_fields)) {
                        other_fields[prefix] = ""
                    }
                    if (other_fields[prefix] != "") {
                        other_fields[prefix] = other_fields[prefix] "\n"
                    }
                    other_fields[prefix] = other_fields[prefix] $i
                }
            }
        }
        
        # Store event data
        for (prefix in other_fields) {
            events_1[event_count_1, prefix] = other_fields[prefix]
        }
        
        # Sort and store data fields
        if (data_count > 0) {
            # Create sorting indices based on subgroup, preserving order within subgroups
            for (i = 1; i <= data_count; i++) {
                sort_indices[i] = i
            }
            
            # Sort indices by subgroup name
            for (i = 1; i <= data_count; i++) {
                for (j = i + 1; j <= data_count; j++) {
                    if (data_subgroups[sort_indices[i]] > data_subgroups[sort_indices[j]]) {
                        temp = sort_indices[i]
                        sort_indices[i] = sort_indices[j]
                        sort_indices[j] = temp
                    }
                }
            }
            
            # Join data fields in sorted order
            sorted_data = ""
            for (i = 1; i <= data_count; i++) {
                if (sorted_data != "") {
                    sorted_data = sorted_data "\n"
                }
                sorted_data = sorted_data data_fields[sort_indices[i]]
            }
            events_1[event_count_1, "data"] = sorted_data
        }
    }
    next
}

# Process second file (actual)
{
    if (NF > 0) {
        event_count_2++
        delete data_fields
        delete other_fields
        data_count = 0
        
        # Parse fields and separate data fields from others
        for (i = 1; i <= NF; i++) {
            if ($i != "") {
                # Extract prefix (everything before first colon)
                prefix = $i
                sub(/:.*/, "", prefix)
                
                if (prefix == "data") {
                    # Extract subgroup (first word after "data: ")
                    subgroup = $i
                    sub(/^data: /, "", subgroup)
                    sub(/ .*$/, "", subgroup)
                    
                    # Store data fields with subgroup for sorting
                    data_count++
                    data_fields[data_count] = $i
                    data_subgroups[data_count] = subgroup
                } else {
                    # Store non-data fields by prefix
                    if (!(prefix in other_fields)) {
                        other_fields[prefix] = ""
                    }
                    if (other_fields[prefix] != "") {
                        other_fields[prefix] = other_fields[prefix] "\n"
                    }
                    other_fields[prefix] = other_fields[prefix] $i
                }
            }
        }
        
        # Store event data
        for (prefix in other_fields) {
            events_2[event_count_2, prefix] = other_fields[prefix]
        }
        
        # Sort and store data fields
        if (data_count > 0) {
            # Create sorting indices based on subgroup, preserving order within subgroups
            for (i = 1; i <= data_count; i++) {
                sort_indices[i] = i
            }
            
            # Sort indices by subgroup name
            for (i = 1; i <= data_count; i++) {
                for (j = i + 1; j <= data_count; j++) {
                    if (data_subgroups[sort_indices[i]] > data_subgroups[sort_indices[j]]) {
                        temp = sort_indices[i]
                        sort_indices[i] = sort_indices[j]
                        sort_indices[j] = temp
                    }
                }
            }
            
            # Join data fields in sorted order
            sorted_data = ""
            for (i = 1; i <= data_count; i++) {
                if (sorted_data != "") {
                    sorted_data = sorted_data "\n"
                }
                sorted_data = sorted_data data_fields[sort_indices[i]]
            }
            events_2[event_count_2, "data"] = sorted_data
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
}

# If script is called directly (not sourced), run the comparison
if [ "${0##*/}" = "compare-sse.sh" ]; then
    compare_sse "$@"
fi