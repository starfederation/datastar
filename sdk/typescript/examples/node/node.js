import { createServer } from "node:http";
import { ServerSentEventGenerator } from "datastar-sdk/node";

const hostname = "127.0.0.1";
const port = 3000;

const server = createServer(async (req, res) => {
  if (req.url === "/") {
    const headers = new Headers({ "Content-Type": "text/html" });
    res.setHeaders(headers);
    res.end(
      `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@release-candidate/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
    );
  } else if (req.url?.includes("/merge")) {
    const reader = await ServerSentEventGenerator.ReadSignals(req);

    if (!reader.success) {
      console.error("Error while reading signals", reader.error);
      res.end(`Error while reading signals`);
      return;
    }

    if (!("foo" in reader.signals)) {
      console.error("The foo signal is not present");

      res.end("The foo signal is not present");
      return;
    }

    ServerSentEventGenerator.stream(req, res, (stream) => {
      stream.patchElements(
        `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
      );
    });
  } else {
    res.end("Path not found");
  }
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
