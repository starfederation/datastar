# SDK tests

This is where the code for the [generic tests](/sdk/test) lives.

## Running the test app

- repl:

```
clojure -M:repl -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"
```

- main:

```
clojure -M -m starfederation.datastar.clojure.sdk-test.main
```
