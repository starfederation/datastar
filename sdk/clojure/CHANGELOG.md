# Release notes for the Clojure SDK

## 2025-02-03

### Changed

- The ring adapter for the SKD is now a generic ring adapter. This adapter
  depends solely on the ring core protocols, the dependency to the ring
  jetty adapter has been removed.

> [!important]
> This change comes with these breaking changes:
>
> - The `adapter-jetty` directory has been renamed `adapter-ring`, this mean a
>   change in your `deps.edn`. Check the readme.
> - The `starfederation.datastar.clojure.adapter.ring-jetty` has been moved to
>   `starfederation.datastar.clojure.adapter.ring`

### Added

- Added a function to the main API that test for the presence of Datastar
  headers in a ring request.
- The `->sse-response` functions from the adapters now have the option to
  modify the HTTP response status code. It is useful when wanting to return
  204 responses for instance.
- rj9a will be supported as soon as there is a fix for the connection lifetime
  issue in asynchronous ring.

### Fixed

- SSE retry duration data line is now properly elided when its value passed
  as an option is equal the default retry duration value from the generated
  constants.
- Fixed miss-management of web-drivers in tests, the drivers are now killed
  when the JVM shuts down
