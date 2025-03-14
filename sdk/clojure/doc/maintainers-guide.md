# Notes to self and potential maintainers

## Directory structure

In the whole Datastar project:

- `examples/clojure`

In the SDK code proper `sdk/clojure`:

- `sdk`: the source folder for the main SDK
- `adapter-*`: source folders for adapter specific code
- `malli-schemas`: self explanatory...
- `src/bb`: tasks used run a repl, tests...
- `src/dev`: dev utils, examples
- `src/test`: centralized tests for all the libraries
- `test-resources`: self explanatory

## bb tasks

### Development tasks `bb run dev`

- `bb run dev`: start a repl with the dev nss, the test nss the malli schemas,
  ring-jetty and Http-kit on the classpath
- `bb run dev:rj9a`: same as basic dev task expect for ring-jetty being replaced
  with rj9a.

> [!note]
> You can add additional deps aliases when calling these tasks:
> `bb run dev :debug` will add a Flowstorm setup

### Test tasks `bb run test`

- `bb run test:all`: run all test for the SDK, the Http-kit adapter and the
  ring adapter using ring-jetty.
- `bb run test:rj9a`: run all test for the SDK and the ring adapter using rj9a.

## Test

### Running tests

- for the unit and smoke tests see the bb tasks.
- for the generic bash SDK tests
  1. go to `sdk/clojure/sdk-tests/`
  2. run `clojure -M -m starfederation.datastar.clojure.sdk-test.main`
  3. go to `sdk/test/`
  4. run `./test-all.sh localhost:8080`

### webdriver config

Tests resources contains a test.config.edn file. It contains a map whose keys
are:

- `:drivers`: [etaoin](https://github.com/clj-commons/etaoin) webdriver types to run
- `:webdriver-opts`: a map of webdriver type to webriver specific options
