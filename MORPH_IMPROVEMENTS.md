# morphing Improvements Ported to Datastar

## Overview

This document describes the improvements ported from my test version of Idiomorph/htmx4 to Datastar's `patchElements.ts` morphing algorithm. These changes reduce DOM churn and improve matching behavior when updating the DOM.

## Key Changes

### 1. Enhanced Node Matching

**New Functions:**
- `isMatch()` - Uses `isEqualNode()` for deep equality check, falls back to matching on identifying attributes (name, href, src)
- `matchesUpcomingSibling()` - Checks if a node matches any upcoming siblings within a limit

**Benefits:**
- Deep equality checking via `isEqualNode()` catches identical nodes
- Attribute-based matching (name, href, src) identifies semantically equivalent elements
- Prevents premature matching when better matches exist later in the DOM
- Reduces unnecessary DOM operations

### 2. Improved `findBestMatch()` Algorithm

**Changes:**
- Added non-element node early exit (text/comment nodes only check first position)
- Replaced sibling soft match counting with scan limit (10 nodes)
- Added exact/attribute matching within scan window before falling back to tag-only matches
- Optimized active element check (moved earlier in loop for better performance)
- Added final check to defer soft match if upcoming siblings will use it better

**Matching Priority:**
1. ID set match (highest priority)
2. Exact or attribute match (within scan window)
3. Tag-only match (fallback)

**Benefits:**
- Better matching accuracy, especially for prepended elements
- Limits how far the algorithm scans to improve performance
- Reduces DOM churn by avoiding poor matches

### 3. Node Preservation Strategy

**New Function:**
- `moveNodesBetweenToEnd()` - Replaces inline removal logic

**Changes:**
- Instead of removing nodes between insertion point and best match, potentially useful nodes are moved to the end for later reuse
- Nodes are preserved if they:
  - Have ID children (contain state)
  - Match upcoming new content

**Benefits:**
- Preserves nodes that might be reused later
- Reduces unnecessary removal and recreation of DOM nodes
- Better state preservation

**New Context Variables:**
- `ctxFutureMatches` - WeakSet tracking nodes that match upcoming siblings
- `ctxActiveElementAndParents` - Array of active element and its ancestors

**Benefits:**
- optimize performance of slow contains Active Element lookup
- Tracks which nodes should be deferred for better matches

## Performance Impact

These improvements result in:
- **Reduced DOM churn** - Fewer unnecessary node removals and recreations
- **Better state preservation** - Nodes with IDs and form state are handled more carefully
- **Smarter matching** - Better heuristics for finding the right node to morph

## Use Cases Improved

1. **Prepending elements** - No longer causes excessive DOM churn
2. **Reordering lists** - Better detection of moved elements
3. **Dynamic content** - More accurate matching of similar elements

## Technical Details

### Scan Limit
The algorithm now scans up to 10 nodes ahead when looking for matches. This balances accuracy with performance.

### Future Match Deferral
If a soft match would be better used by an upcoming sibling, the algorithm returns null to insert the current node instead, allowing the soft match to be used later.

### Deep Equality Checking
The `isMatch()` function first uses `isEqualNode()` which performs a deep comparison of nodes including their attributes, children, and content. This catches exact duplicates before falling back to attribute-based heuristics.

## Source

These improvements are based on recent enhancements to the Idiomorph algorithm that has been worked on for htmx4 based on the ideas from morphlex, which Datastar's morphing algorithm is derived from.
