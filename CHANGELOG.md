# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.11

### Added

- Added the `__viewtransition` modifier to `data-on-interval`.
- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-intersect`.
- Added the `__viewtransition` modifier to `data-on-load`.
- Added the `__viewtransition` modifier to `data-on-raf`.
- Added the `__debounce`, `__throttle` and `__viewtransition` modifiers to `data-on-signal-change`.

### Changed

- The `datastar-sse` event is now dispatched on the `document` element, and using `data-on-datastar-sse` automatically listens for the event on the `document` ([#802](https://github.com/starfederation/datastar/issues/802)).
- The `data-on-signals-change-*` attribute key now accepts a path in which `*` can be used as a wildcard (`data-on-signals-change-foo.*`).
- The `@setAll` action now accepts one or more space-separated paths in which `*` can be used as a wildcard (`@setAll('foo.* bar.*', true)`).
- The `@toggleAll` action now accepts one or more space-separated paths in which `*` can be used as a wildcard (`@toggleAll('foo.* bar.*', true)`).