import { createServer } from "node:http";
import { ServerSentEventGenerator } from "./serverSentEventGenerator";

const hostname = '127.0.0.1';
const port = 3000;

const server = createServer((req, res) => {
    console.log(req.url);
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
});

server.listen(port, hostname, () => {
	console.log(`Server running at http://${hostname}:${port}/`);
});

