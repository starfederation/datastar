# Release notes for the Clojure SDK

## 2025-06-22

### Changed

- The public API has seen it's main functions renamed following the new SDK ADR.
  Several functions have been renamed or removed:

  | Old                 | new                   |
  | ------------------- | --------------------- |
  | `merge-fragment!`   | `patch-elements!`     |
  | `merge-fragments!`  | `patch-elements-seq!` |
  | `remove-fragments!` | `remove-element!`     |
  | `merge-signals!`    | `patch-signals!`      |
  | `remove-signals`    | removed               |

- All the examples and snippets have been updated following the ADR changes.

### Fixed

- A superflous newline character was send when marking the end of a SSE event
- The clj-kondo config file for the SDK has been moved in a
  `clj-kondo.exports/starfederation.datastar.clojure/sdk` directory. This change
  allows for other projects to use
  `starfederation.datastar.clojure/XXXX/config.edn` for their clj-kondo config.

### Added

- There is a new http-kit API that allows a more natural ring response model when
  using SSE. With the current API the status and headers for a response are
  sent directly while `->sse-response` is running, the `on-open` callback runs
  just after. For instance that any middleware that would add headers after the
  execution of the `->sse-response` function won't work, the initial response
  being already sent.
  The new `starfederation.datastar.clojure.adapter.http-kit2`
  API changes this behavior. In this new api the initial response is not sent
  during `->sse-response`. Instead a middleware takes care of sending it and
  only then calls the `on-open` callback. If this middleware is the last to run
  on the return any addition to the response map will be taken into account.
- A new library providing Brotli write profile has been added.

## 2025-04-07

### Added

- The vars holding keywords (like `on-open`) in the adapter namespaces were not
  properly recognized by Clj-kondo. This generated `unresolved var` warnings. A
  specific Clj-kondo config has been added to fix these warnings.

### Fixed

- The HTTP version detection that determines whether to add the
  `Connection keep-alive` HTTP header has been changed. The header is now
  properly added for versions older than `HTTP/1.1`.

### Changed

- Bumped the ring version from `1.13.0` to `1.14.1`. This encourages users
  to use Jetty 12 when using ring jetty adapter.

## 2025-03-31

### Changed

- Removed the use of the deprecated `:on-open` and `:on-close` keywords. The
  `->sse-response` functions of both adapters will not use them anymore. The
  corresponding docstrings are updated.

## 2025-03-11

### Deprecated

- the use of `:on-open` and `:on-close` keywords for the `->sse-response`
  function is deprecated and will be removed in a future release.
  See `->sse-response`'s docstring.

## 2025-03-11

### Added

- Added a "write profile" mechanism that allows user to use compression of
  SSE streams and let them control the IO buffering behavior.
- Added new malli schemas to go with the "write profiles".
- Added the ability to pass an on-error callback to SSE generators, it allows
  for changing or augmenting the behavior on exceptions.
- `starfederation.datastar.clojure.api/lock-sse!`. This is a macro allowing
  its body to be protected by a SSE generator's lock.
- Added a Flowstorm setup to help debugging when working on the SDK.

### Fixed

- Fixed a typo in the cache-control HTTP header.
- Fixed some malli schemas
- There were problems with the handling of concurrent uses of the ring adapter.
  This adapter uses a re-entrant lock to prevent some bad behaviors and the
  previous implementation was too simplistic. The lock management has been
  redone and there are now some basic tests for the hairiest part of the lock's
  handling.

### Changed

- The handling of errors and the management of concurrency for the adapters has
  been redesigned. The ring implementation particularly needed some work.
  Now when a `IOException` is thrown sending an event, adapters will close
  themselves. Other exceptions are rethrown. When trying to close an already
  closed SSE generator, the `close-sse!` function just returns `false`.
  Previously the ring adapter was potentially throwing in that case.
  Also the `on-close` callback can be called at most once.
  Both adapters behave similarly when it comes to errors.
- A slight change to the `starfederation.datastar.clojure.api.sse` namespace
  makes it a generic SSE event formatter.
  - `starfederation.datastar.clojure.sse/write-event!` is now Datastar agnostic
  - `starfederation.datastar.clojure.sse/headers` is now a generic function
    to make HTTP headers containing the SSE specific ones.
- The SSE headers added by the SDK no longer override the user provided ones.
- bumped http-kit version: `2.9.0-alpha2` -> `2.9.0-alpha4`

### Docs

- Several docstrings, and documentation files have been corrected/added.
- Added documentation giving the rationale for the "write profile" concept
  and giving some usage examples.
- Added the load_more code snippet to the main site's code snippets

## 2025-02-15

### Added

- `starfederation.datastar.clojure.adapter.test/->sse-response`. This is a mock
  for a SSE ring response that records the SSE events sent with it.
- Example snippets for the main site, ie, polling and redirection. These
  examples are runnable from the development examples.
- Development example of the usage of the redirect sugar.

### Fixed

- Fixed the main readme example (wrong arity of `:on-close` callback using http-kit)
- The jetty adapter now returns a harmless value when sending an event. It used
  to return the write buffer which shouldn't be used directly.
- The `starfederation.datastar.clojure.api/redirect!` helper function uses a js
  timeout for redirection

## 2025-02-03

### Changed

- The ring adapter for the SDK is now a generic ring adapter. This adapter
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
