# Datastar http-kit adapter

## Installation

For now the SDK and adapters are distributed as git dependencies using a `deps.edn` file.

```clojure
{datastar/sdk {:git/url "https://github.com/starfederation/datastar/"
               :git/sha "LATEST SHA"
               :deps/root "sdk/clojure/sdk"}

 datastar/http-kit {:git/url "https://github.com/starfederation/datastar/"
                    :git/sha "LATEST SHA"
                    :deps/root "sdk/clojure/adapter-http-kit"}}
```

> [!important]
> Replace `LATEST_SHA` in the git coordinates below by the actual latest commit sha of the repository.
