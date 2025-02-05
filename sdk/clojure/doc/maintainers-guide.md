# Notes to self and potential maintainers

## Directory structure

In the whole project:

- `examples/clojure`

In the sdk code proper `sdk/clojure`:

- `sdk`: the source folder for the main sdk
- `adapter-*`: source folders for adapter specific code
- `malli-schemas`: self explanatory...
- `src/bb`: tasks used run a repl, tests...
- `src/dev`: dev utils, examples
- `src/test`: centralized tests for all the libraries
- `test-resources`: self explanatory

## bb tasks

- `bb run dev`: start a repl with all the sub projects in the classpath
- `bb run test:all`: run all test for sdk, adapters...

## Test

- Tests resources contains a test.config.edn file. It contains a map whose keys are:
  - `:drivers`: [etaoin](https://github.com/clj-commons/etaoin) webdriver types to run
  - `:webdriver-opts`: a map of webdriver type to webriver specific options
