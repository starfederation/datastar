# Malli schemas for the SDK

## Installation

For now the SDK and adapters are distributed as git dependencies using a `deps.edn` file.

```clojure
{datastar/malli-schemas {:git/url "https://github.com/starfederation/datastar/"
                         :git/sha "LATEST SHA"
                         :deps/root "sdk/clojure/malli-schemas"}}
```

> [!important]
> Replace `LATEST_SHA` in the git coordinates below by the actual latest commit sha of the repository.

## Usage

Require the namespaces for which you want schema and/or instrumentation. Then
use malli's instrumentation facilities.

Notable schema namespaces:

- `starfederation.datastar.clojure.api-schemas` for the general d\* API
- `starfederation.datastar.clojure.api.*-schemas` for more specific code underlying the main API
- `starfederation.datastar.clojure.adapter.common-schemas` for the common adapter machinery (write profiles)
- `starfederation.datastar.clojure.adapter.http-kit-schemas` for the http-kit adapter
- `starfederation.datastar.clojure.adapter.ring-schemas` for the ring adapter
