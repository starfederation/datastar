# Generic Clojure SDK for Datastar

This is where the code for the Generic SDK lives.

## Instalation

For now the SDK and adapters are distributed as git dependencies using a `deps.edn` file.
If you roll your own adapter you only need:

```clojure
{datastar/sdk {:git/url "https://github.com/starfederation/datastar/tree/develop"
              :git/sha "LATEST SHA"
              :deps/root "sdk/clojure/sdk"}}
```

> [!important]
> This project is new and there isn't a release process yet other than using git shas.
> Replace `LATEST_SHA` in the git coordinates below by the actual latest commit sha of the repository.
