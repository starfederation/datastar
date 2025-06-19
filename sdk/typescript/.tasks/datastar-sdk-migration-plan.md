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
- [ ] Update `src/abstractServerSentEventGenerator.ts`
  - [ ] Rename `mergeFragments()` method → `PatchElements()`
  - [ ] Rename `mergeSignals()` method → `PatchSignals()`
  - [ ] Update method signatures to match new specification
  - [ ] Change internal event type calls to use new constants
  - [ ] Update data line generation for `elements` instead of `fragments`
  - [ ] Update data line generation to use `DatastarDatalinePatchMode` instead of `DatastarDatalineMergeMode`
  - [ ] Remove `removeFragments()` method (functionality moved to `PatchElements` with `remove` mode)
  - [ ] Remove `removeSignals()` method (not in new specification)
  - [ ] Remove `executeScript()` method (replaced by `ExecuteScript` via `PatchElements`)

### Phase 2: Implementation Updates

#### 2.1 Runtime-Specific Implementations
- [ ] Update `src/node/serverSentEventGenerator.ts`
  - [ ] Verify constructor and streaming logic compatibility
  - [ ] Ensure `readSignals` method follows new specification requirements
  - [ ] Test HTTP method handling (GET vs POST)
  - [ ] Update error handling to match specification

- [ ] Update `src/web/serverSentEventGenerator.ts`
  - [ ] Verify constructor and streaming logic compatibility  
  - [ ] Ensure `readSignals` method follows new specification requirements
  - [ ] Test HTTP method handling (GET vs POST)
  - [ ] Update error handling to match specification

### Phase 3: New Methods Implementation

#### 3.1 Add New Methods to Abstract Class
- [ ] Implement `ExecuteScript()` method in `abstractServerSentEventGenerator.ts`
  - [ ] Method should create a `<script>` element via `PatchElements`
  - [ ] Support `autoRemove` option with `data-on-load="el.remove()"`
  - [ ] Support `attributes` option for script tag attributes
  - [ ] Use `append` mode and `body` selector by default
  - [ ] Handle multi-line scripts properly

#### 3.2 Parameter Validation and Defaults
- [ ] Add validation for new `ElementPatchMode` values
- [ ] Implement proper defaults according to specification:
  - [ ] Default `mode`: `outer`
  - [ ] Default `useViewTransition`: `false`
  - [ ] Default `onlyIfMissing`: `false`
  - [ ] Default `autoRemove`: `false` (for ExecuteScript)
- [ ] Add parameter validation for required fields

### Phase 4: Testing and Examples

#### 4.1 Update Examples
- [ ] Update `examples/node/node.js`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable

- [ ] Update `examples/deno/deno.ts`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable

- [ ] Update `examples/bun/bun.ts`
  - [ ] Change `mergeFragments` → `PatchElements`
  - [ ] Update method calls to use new parameter names
  - [ ] Test with new element modes if applicable

#### 4.2 Test Suite Updates
- [ ] Update existing tests in `test/` directory
  - [ ] Change method names in test cases
  - [ ] Update expected event types in assertions
  - [ ] Add tests for new `ElementPatchMode` values
  - [ ] Add tests for `ExecuteScript` functionality
  - [ ] Test parameter validation
  - [ ] Test default value behavior

- [ ] Add new test cases for specification compliance
  - [ ] Test `remove` mode with and without selector
  - [ ] Test nested element patching
  - [ ] Test view transition integration
  - [ ] Test script execution with autoRemove
  - [ ] Test signal patching with onlyIfMissing

### Phase 5: Documentation and Build

#### 5.1 Documentation Updates
- [ ] Update `README.md`
  - [ ] Change all method examples from `mergeFragments` → `PatchElements`
  - [ ] Change all method examples from `mergeSignals` → `PatchSignals`
  - [ ] Update API reference section
  - [ ] Add documentation for new `ExecuteScript` method
  - [ ] Update parameter documentation
  - [ ] Add examples of new element patch modes

- [ ] Update inline code documentation (JSDoc comments)
  - [ ] Update method descriptions
  - [ ] Update parameter descriptions
  - [ ] Add examples for new functionality

#### 5.2 Build and Package Updates
- [ ] Update `build.ts` if necessary for new file structure
- [ ] Update `package.json` version and descriptions
- [ ] Verify Deno, Node.js, and Bun compatibility
- [ ] Test build process for all targets

### Phase 6: Validation and Testing

#### 6.1 Integration Testing
- [ ] Test with actual Datastar frontend library
- [ ] Verify SSE event format matches expected client-side parsing
- [ ] Test all element patch modes (`outer`, `inner`, `replace`, `prepend`, `append`, `before`, `after`, `remove`)
- [ ] Test view transitions integration
- [ ] Test script execution and auto-removal

#### 6.2 Cross-Runtime Testing
- [ ] Test Node.js implementation with examples
- [ ] Test Deno implementation with examples  
- [ ] Test Bun implementation with examples
- [ ] Verify consistent behavior across all runtimes

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