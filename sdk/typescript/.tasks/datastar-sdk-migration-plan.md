# Datastar TypeScript SDK Migration Plan

## Overview
This document outlines the step-by-step migration from the current Datastar TypeScript SDK to the new specification. The new spec introduces significant changes in naming conventions, API structure, and functionality.

## Key Changes Summary
1. **Method Naming**: `mergeFragments` → `PatchElements`, `mergeSignals` → `PatchSignals`
2. **Event Types**: `datastar-merge-fragments` → `datastar-patch-elements`, `datastar-merge-signals` → `datastar-patch-signals`
3. **Terminology**: "Fragments" → "Elements", "Merge" → "Patch"
4. **New Modes**: `morph` mode split into `outer` and `inner`, new modes added
5. **Simplified API**: Reduced complexity in some areas, more structured options
6. **Parameter Changes**: `fragments` → `elements`, `DatastarDatalineMergeMode` → `DatastarDatalinePatchMode` (value: `"mode"`)

---

## Migration Tasks

### Phase 1: Core Infrastructure Updates

#### 1.1 Update Constants and Types
- [x] Update `src/consts.ts` to match new specification
  - [x] Change event types: `datastar-merge-fragments` → `datastar-patch-elements`
  - [x] Change event types: `datastar-merge-signals` → `datastar-patch-signals`
  - [x] Update `FragmentMergeModes` to `ElementPatchModes` with new values: `outer`, `inner`, `replace`, `prepend`, `append`, `before`, `after`, `remove`
  - [x] Remove `morph` mode (split into `outer`/`inner`)
  - [x] Remove `upsertAttributes` mode (not in new spec)
  - [x] Change dataline constants: `DatastarDatalineFragments` → `DatastarDatalineElements`, `DatastarDatalineMergeMode` → `DatastarDatalinePatchMode` (value: `"mode"`)
  - [x] Update default mode from `morph` → `outer`
  - [x] Set `DefaultExecuteScriptAutoRemove = false` (per new spec)

- [x] Update `src/types.ts` to reflect new specification
  - [x] Rename `FragmentMergeMode` → `ElementPatchMode`
  - [x] Update `MergeFragmentsOptions` → `PatchElementsOptions`
  - [x] Update `MergeSignalsOptions` → `PatchSignalsOptions`
  - [x] Remove fragment-specific events, add element-specific events
  - [x] Update parameter names in interfaces to use `DatastarDatalinePatchMode`
  - [x] Replace `npm:type-fest` dependency with local `Jsonifiable` type

#### 1.2 Abstract Class Core Changes
- [x] Update `src/abstractServerSentEventGenerator.ts`
  - [x] Rename `mergeFragments()` method → `PatchElements()`
  - [x] Rename `mergeSignals()` method → `PatchSignals()`
  - [x] Update method signatures to match new specification
  - [x] Change internal event type calls to use new constants
  - [x] Update data line generation for `elements` instead of `fragments`
  - [x] Update data line generation to use `DatastarDatalinePatchMode` instead of `DatastarDatalineMergeMode`
  - [x] Remove `removeFragments()` method (functionality moved to `PatchElements` with `remove` mode)
  - [x] Remove `removeSignals()` method (not in new specification)
  - [x] Remove `executeScript()` method (replaced by `ExecuteScript` via `PatchElements`)
  - [x] Updated all imports to use new types (`PatchElementsOptions`, `PatchSignalsOptions`, etc.)
  - [x] Fixed dataline spacing (removed extra space in concatenation)
  - [x] Implemented `ExecuteScript()` method (moved from Phase 3.1)
  - [x] Updated JSDoc comments for all methods
  - [x] Complete class rewrite for full spec compliance

### Phase 2: Implementation Updates

#### 2.1 Runtime-Specific Implementations
- [x] Update `src/node/serverSentEventGenerator.ts`
  - [x] Verify constructor sets required headers per spec: `Cache-Control: no-cache`, `Content-Type: text/event-stream`, `Connection: keep-alive` (HTTP/1.1 only) ✓ (using `sseHeaders`)
  - [x] Ensure immediate flush after setting headers (per spec requirement) ✓ (added `res.flushHeaders()`)
  - [x] Update `readSignals` method to handle GET requests with `datastar` query parameter as URL-encoded JSON ✓ (already implemented correctly)
  - [x] Update `readSignals` method to handle non-GET requests by parsing request body as JSON ✓ (improved validation)
  - [x] Ensure error handling follows spec requirement: "Must return error for invalid JSON" ✓ (proper error handling with Record validation)
  - [x] Replace `npm:type-fest` import with local `Jsonifiable` type (from types.ts) ✓
  - [x] Verify ordered delivery mechanism (thread safety requirement) ✓ (Node.js single-threaded nature provides ordering)

- [x] Update `src/web/serverSentEventGenerator.ts`
  - [x] Verify constructor sets required headers per spec: `Cache-Control: no-cache`, `Content-Type: text/event-stream`, `Connection: keep-alive` (HTTP/1.1 only) ✓ (using `sseHeaders` via deepmerge)
  - [x] Ensure immediate flush after setting headers (per spec requirement) ✓ (Web API handles this automatically)
  - [x] Update `readSignals` method to handle GET requests with `datastar` query parameter as URL-encoded JSON ✓ (already implemented correctly)
  - [x] Update `readSignals` method to handle non-GET requests by parsing request body as JSON ✓ (already implemented correctly)
  - [x] Ensure error handling follows spec requirement: "Must return error for invalid JSON" ✓ (proper error handling with Record validation)
  - [x] Replace `type-fest` import with local `Jsonifiable` type (from types.ts) ✓
  - [x] Verify ordered delivery mechanism (thread safety requirement) ✓ (Web Streams API provides ordering guarantees)

### Phase 3: New Methods Implementation

#### 3.1 Add New Methods to Abstract Class
- [x] Implement `ExecuteScript()` method in `abstractServerSentEventGenerator.ts`
  - [x] Method creates a `<script>` element via `PatchElements`
  - [x] Supports `autoRemove` option with `data-on-load="el.remove()"`
  - [x] Supports `attributes` option for script tag attributes
  - [x] Uses `append` mode and `body` selector by default
  - [x] Handles multi-line scripts properly

#### 3.2 Parameter Validation and Defaults
- [x] Verify defaults match specification requirements:
  - [x] Default `mode`: `outer` (ElementPatchMode) ✓ (verified in constants)
  - [x] Default `useViewTransition`: `false` ✓ (verified in constants) 
  - [x] Default `onlyIfMissing`: `false` ✓ (verified in constants)
  - [x] Default `autoRemove`: `false` (for ExecuteScript) ✓ (verified in constants)
  - [x] Default `retryDuration`: `1000` (milliseconds) ✓ (verified in constants)
- [x] Add validation for `ElementPatchMode` enum values in abstract class ✓ (added `validateElementPatchMode`)
- [x] Add validation for required fields (elements parameter, signals parameter, script parameter) ✓ (added `validateRequired`)
- [x] Add validation for selector format and requirements per spec ✓ (added validation for remove mode selector logic)
- [x] Ensure proper JSON validation for signals parameter ✓ (added `validateJSON`)
- [x] Validate script attributes format and allowed values ✓ (added `validateScriptAttributes`)

### Phase 4: Test Suite Updates

#### 4.1 Update Existing Tests
- [ ] Update existing tests in `test/` directory
  - [ ] Change method names in test cases (`mergeFragments` → `PatchElements`, etc.)
  - [ ] Update expected event types in assertions
  - [ ] Update parameter names to use new constants
  - [ ] Test parameter validation functionality
  - [ ] Test default value behavior

#### 4.2 Add New Test Cases for Specification Compliance
- [ ] Add tests for new `ElementPatchMode` values
  - [ ] Test `outer`, `inner`, `replace`, `prepend`, `append`, `before`, `after`, `remove` modes
  - [ ] Test `remove` mode with and without selector
  - [ ] Test nested element patching
- [ ] Add tests for `ExecuteScript` functionality
  - [ ] Test script execution with and without autoRemove
  - [ ] Test script attributes handling
  - [ ] Test multi-line scripts
- [ ] Add tests for `PatchSignals` functionality
  - [ ] Test signal patching with onlyIfMissing
  - [ ] Test JSON validation
  - [ ] Test RFC 7386 JSON Merge Patch semantics
- [ ] Add parameter validation tests
  - [ ] Test validation error cases
  - [ ] Test edge cases and boundary conditions

### Phase 5: Examples and Documentation Updates

#### 5.1 Update Examples (After Tests Pass)
- [ ] Update `examples/node/node.js`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Change `mergeSignals` → `PatchSignals`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable
  - [ ] Add examples of `ExecuteScript` usage

- [ ] Update `examples/deno/deno.ts`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Change `mergeSignals` → `PatchSignals`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable
  - [ ] Add examples of `ExecuteScript` usage

- [ ] Update `examples/bun/bun.ts`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Change `mergeSignals` → `PatchSignals`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable
  - [ ] Add examples of `ExecuteScript` usage

#### 5.2 Documentation Updates
- [ ] Update `README.md`
  - [ ] Change all method examples from `mergeFragments` → `PatchElements`
  - [ ] Change all method examples from `mergeSignals` → `PatchSignals`
  - [ ] Update API reference section
  - [ ] Add documentation for new `ExecuteScript` method
  - [ ] Update parameter documentation
  - [ ] Add examples of new element patch modes
  - [ ] Document parameter validation behavior

- [ ] Update inline code documentation (JSDoc comments)
  - [ ] Update method descriptions
  - [ ] Update parameter descriptions
  - [ ] Add examples for new functionality
  - [ ] Document validation error scenarios

### Phase 6: Build and Final Validation

#### 6.1 Build and Package Updates
- [ ] Update `build.ts` if necessary for new file structure
- [ ] Update `package.json` version and descriptions
- [ ] Verify Deno, Node.js, and Bun compatibility
- [ ] Test build process for all targets

#### 6.2 Integration Testing
- [ ] Test with actual Datastar frontend library
- [ ] Verify SSE event format matches expected client-side parsing
- [ ] Test all element patch modes in browser environment
- [ ] Test view transitions integration
- [ ] Test script execution and auto-removal in browser

#### 6.3 Cross-Runtime Testing
- [ ] Test Node.js implementation with updated examples
- [ ] Test Deno implementation with updated examples  
- [ ] Test Bun implementation with updated examples
- [ ] Verify consistent behavior across all runtimes
- [ ] Performance testing and validation

---

## Breaking Changes for Users

### Method Name Changes
- `stream.mergeFragments()` → `stream.PatchElements()`
- `stream.mergeSignals()` → `stream.PatchSignals()`

### Parameter Changes
- Options object property: Uses `DatastarDatalinePatchMode` constant (value: `"mode"`) instead of `DatastarDatalineMergeMode`
- Mode values: `morph` → `outer` (recommended) or `inner`
- Dataline constants: `DatastarDatalineFragments` → `DatastarDatalineElements`

### Removed Methods
- `stream.removeFragments()` → Use `stream.PatchElements()` with `mode: 'remove'`
- `stream.removeSignals()` → Not available in new specification
- `stream.executeScript()` → Use `stream.ExecuteScript()`

### New Functionality
- New element patch modes: `replace`, `remove`
- New `ExecuteScript()` method with auto-removal support
- Enhanced view transition support

---

## Risk Assessment

### High Risk
- [ ] Breaking changes in public API require major version bump
- [ ] Event type changes require frontend library compatibility verification

### Medium Risk  
- [ ] Parameter validation changes might break existing invalid usage
- [ ] Default value changes might alter behavior

### Low Risk
- [ ] Internal implementation changes
- [ ] Documentation updates
- [ ] Example updates

---

## Completion Criteria

- [ ] All tests pass across Node.js, Deno, and Bun
- [ ] Examples work with updated API
- [ ] Documentation accurately reflects new specification
- [ ] No breaking changes beyond those documented above
- [ ] Performance characteristics maintained or improved
- [ ] Full compliance with new Datastar SDK specification 