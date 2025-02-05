# Malli schemas for the SDK

## Installation

For now the SDK and adapters are distributed as git dependencies using a `deps.edn` file.

```clojure
{datastar/malli-schemas {:git/url "https://github.com/starfederation/datastar/tree/develop"
                         :git/sha "LATEST SHA"
                         :deps/root "sdk/clojure/malli-schemas"}}
```

> [!important]
> Replace `LATEST_SHA` in the git coordinates below by the actual latest commit sha of the repository.
