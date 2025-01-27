# Typescript SDK for Datastar

Implements the [SDK spec](../README.md) and exposes an abstract
ServerSentEventGenerator class that can be used to implement runtime specific
classes. NodeJS and web standard runtimes are currently implemented.

Currently it only exposes an http1 server, if you want http2 I recommend you use
a reverse proxy until http2 support is added.

Deno is used for building the npm package: `deno run -A build.ts VERSION`

Usage is straightforward:

```
// this example is for node
const reader = await ServerSentEventGenerator.readSignals(req);

if (!reader.success) {
    console.error('Error while reading signals', reader.error);
    res.end('Error while reading signals`);
    return;
}

if (!('foo' in reader.signals)) {
    console.error('The foo signal is not present');
    res.end('The foo signal is not present');
    return;
}

ServerSentEventGenerator.stream(req, res, (stream) => {
     stream.mergeFragments(`<div id="toMerge">Hello ${reader.signals.foo}</div>`);
});
```

## Examples

Follow the links for more complete (and executable) examples

- [NodeJS](./examples/node.ts)
- [Deno](./examples/deno.ts)

## Frameworks / Alternate runtimes

If you can't simply use the node / web versions, then you can extend the abstract
class in `./src/abstractServerSentEventGenerator.ts`. You will need to provide
implementations of the `constructor`, `readSignals`, `stream` and `send`
methods.

## Testing

A shell based testing suite is provided; see the [readme](../test/README.md) for
more information.

### Testing node

Start by building and running the node server

```
$ deno run -A build.ts xxx
$ node ./npm/esm/node/node.js
```

Then run the test suite

```
$ cd ../test
$ ./test-all.sh http://127.0.0.1:3000
Running tests with argument: http://127.0.0.1:3000
Processing GET cases...
Processing POST cases...
```

### Testing deno

Start by running the deno server

```
$ deno --allow-net  ./src/web/deno.ts
```

Then run the test suite

```
$ cd ../test
$ ./test-all.sh http://localhost:8000/
Running tests with argument: http://localhost:8000/
Processing GET cases...
Processing POST cases...
```
