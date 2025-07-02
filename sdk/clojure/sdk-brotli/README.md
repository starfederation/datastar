# Datastar Brotli write profile

This library contains some utilities to work with Brotli.

Credits to [Anders]'(https://andersmurphy.com/) and his work on [Hyperlith](https://github.com/andersmurphy/hyperlith)
from which this library takes it's code.

## Installation

```clojure
{datastar/brotli {:git/url "https://github.com/starfederation/datastar/"
                  :git/sha "LATEST SHA"
                  :deps/root "sdk/clojure/sdk-brotli"}

```

> [!important]
>
> - Replace `LATEST_SHA` in the git coordinates below by the actual latest
>   commit sha of the repository.
> - You need to add a dependency for your specific plateform see the [Brotli4j page](https://github.com/hyperxpro/Brotli4j)
>   For instance on linux `com.aayushatharva.brotli4j/native-linux-x86_64 {:mvn/version "1.18.0"}}}`
