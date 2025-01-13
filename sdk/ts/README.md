# Typescript SDK for Datastar

Implements `../README.md` and exposes a ServerSentEventGenerator class that can be used to
send datastar events to the client.

## NodeJS

Code running on NodeJS should use the `./src/node/serverSentEventGenerator.ts` class.

Here's a simple example:

```
import { createServer } from "node:http";
import { ServerSentEventGenerator } from "./serverSentEventGenerator";

const hostname = '127.0.0.1';
const port = 3000;

const server = createServer(async (req, res) => {
    if (req.url === "/") {
        const headers = new Headers({ 'Content-Type': 'text/html' });
        res.setHeaders(headers);
        res.end(`<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head><body><div id="toMerge" data-on-load="sse('/merge', {method: 'get'})">Not merged</div></body></html>`);
    }
    else if (req.url?.includes("/merge")) {
        ServerSentEventGenerator.stream(req, res, (stream) => {
            stream.mergeFragments('<div id="toMerge">Merged</div>');
        });
    }

    res.end('Path not found');
});

server.listen(port, hostname, () => {
	console.log(`Server running at http://${hostname}:${port}/`);
});
```

## Web Standard runtimes

Code running on platforms that implement web standards (like Deno), should use the `./src/web/serverSentEventGenerator.ts` class.

Here's an example using Deno:

```
import { serve } from "https://deno.land/std@0.140.0/http/server.ts";
import { ServerSentEventGenerator } from "./serverSentEventGenerator.ts";

serve(async (req: Request) => {
    const url = new URL(req.url);

    if (url.pathname === '/') {
        return new Response(
            `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head><body><div id="toMerge" data-on-load="sse('/merge', {method: 'get'})">Not merged</div></body></html>`
            , {
                headers: { 'Content-Type': 'text/html' }
         });
    }
    else if (url.pathname.includes('/merge')) {
        return ServerSentEventGenerator.stream(async (stream) => {
            stream.mergeFragments('<div id="toMerge">Merged</div>');
        });
    }

    return new Response(`Path not found: ${req.url}`, {
        headers: { 'Content-Type': 'text/html' }
    });
});
```

## Frameworks / Alternate runtimes

If you cant simply use the node / web versions, then you can extend the abstract class in `./src/abstractServerSentEventGenerator.ts`. You will need to provide implementations of the `constructor`, `readSignals`, `stream` and `send` methods.

## Testing

A shell based testing suite is provided; see `../test/README.md` for more information.

### Testing node

Start by building and running the node server

```
$ task -f build
$ node ../../bundles/node/node.js
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
$ deno --unstable-sloppy-imports --allow-net  ./src/web/deno.ts
```

Then run the test suite

```
$ cd ../test
$ ./test-all.sh http://localhost:8000/
Running tests with argument: http://localhost:8000/
Processing GET cases...
Processing POST cases...
```
