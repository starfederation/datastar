# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.11

### Added

- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-intersect`.
- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-signal-change`.
- Added the `__viewtransition` modifier to `data-on-interval`.
- Added the `__viewtransition` modifier to `data-on-load`.
- Added the `__viewtransition` modifier to `data-on-raf`.

### Changed

- The `datastar-sse` event is now dispatched on the `document` element, and using `data-on-datastar-sse` automatically listens for the event on the `document` ([#802](https://github.com/starfederation/datastar/issues/802)).
- The `data-on-signals-change-*` attribute key now accepts a path in which `*` matches a single path segment and `**` matches multiple path segments (`data-on-signals-change-foo.*.baz`).
- The `@setAll` action now accepts one or more space-separated paths in which `*` matches a single path segment and `**` matches multiple path segments (`@setAll('foo.*.baz', true)`) ([#793](https://github.com/starfederation/datastar/issues/793)).
- The `@toggleAll` action now accepts one or more space-separated paths in which `*` matches a single path segment and `**` matches multiple path segments (`@toggleAll('foo.*.baz', true)`) ([#793](https://github.com/starfederation/datastar/issues/793)).