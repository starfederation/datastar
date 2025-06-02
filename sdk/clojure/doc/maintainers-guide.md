# Maintainers Guide

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

### Release tasks

- `bb bump-version patch/minor/major`: to bump a version component across all libs
- `bb set-version x.x.x`: to set the version component across all libs
- `bb jar:all`: Build jars artifacts for all of the libs
- `bb jar:<lib-name>`: Build jars artifacts for one of the libs
- `bb clean`: Clean all build artifacts
- `bb publish:all`: Publish the artifacts to clojars.org

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

## Release

- The library artifacts are published to Clojars (http://clojars.org) under the `dev.data-star` namespace.
- The Clojars account is managed by Ben Croker, the DNS verification is managed by Delaney.
- The Clojars deploy token is also managed by Ben and added to this repo as a GH Actions Secret
  - Secret name: `CLOJARS_USERNAME`
    Value: *the clojars account username*
  - Secret name: `CLOJARS_PASSWORD`
    Value: *the clojars deploy token*
- The libraries' versions are bumped in lockstep so that there is no confusion over which version of the common lib should be used with an adapter lib.

The Github Actions [CI workflow for clojure](../../.github/workflows/clojure-sdk.yml) will always run the tests and produce jar artifacts.

Triggering a deployment to clojars is a manual process. A Datastar core contributor must trigger the Clojure SDK workflow with the `publish` input boolean set to `true.

**Release process:**

1. Use `bb set-version` or `bb bump-version` to update the library versions in lockstep
2. Commit those changes and push to GitHub
3. A core contributor must trigger the workflow manually setting `publish` to `true`

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
