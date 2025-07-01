import { createServer } from "node:http";
import { ServerSentEventGenerator } from "../src/node/serverSentEventGenerator.ts";
import type { Jsonifiable } from "../src/types.ts";

const hostname = "127.0.0.1";
const port = 3000;

// This server is used for testing the node sdk
const server = createServer(async (req, res) => {
  if (req.url === "/") {
    res.setHeader("Content-Type", "text/html");
    res.end(
      `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@release-candidate/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
    );
  } else if (req.url?.includes("/merge")) {
    const reader = await ServerSentEventGenerator.readSignals(req);

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
      stream.PatchElements(
        `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
      );
    });
  } else if (req.url?.includes("/test")) {
    const reader = await ServerSentEventGenerator.readSignals(req);
    if (reader.success) {
      const events = reader.signals.events;
      if (isEventArray(events)) {
        ServerSentEventGenerator.stream(req, res, (stream) => {
          testEvents(stream, events);
        });
      }
    } else {
      res.end(reader.error);
    }
  } else if (req.url?.includes("/await")) {
    ServerSentEventGenerator.stream(req, res, async (stream) => {
      stream.PatchElements('<div id="toMerge">Merged</div>');
      await delay(5000);
      stream.PatchElements('<div id="toMerge">After 10 seconds</div>');
    });
  } else {
    res.end("Path not found");
  }
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});

function delay(milliseconds: number) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

function isEventArray(
  events: unknown,
): events is (Record<string, Jsonifiable> & { type: string })[] {
  return events instanceof Array && events.every((event) => {
    return typeof event === "object" && event !== null &&
      typeof event.type === "string";
  });
}

function testEvents(
  stream: ServerSentEventGenerator,
  events: Record<string, Jsonifiable>[],
) {
  events.forEach((event) => {
    const { type, ...e } = event;
    switch (type) {
      case "mergeFragments":
        if (e !== null && typeof e === "object" && "fragments" in e) {
          const { fragments, ...options } = e;
          stream.PatchElements(fragments as string, options || undefined);
        }
        break;
      case "removeFragments":
        if (e !== null && typeof e === "object" && "selector" in e) {
          const { selector, ...options } = e;
          stream.PatchElements(selector as string, options || undefined);
        }
        break;
      case "mergeSignals":
        if (e !== null && typeof e === "object" && "signals" in e) {
          const { signals, ...options } = e;
          stream.PatchSignals(
            JSON.stringify(signals),
            options || undefined,
          );
        }
        break;
      case "removeSignals":
        if (e !== null && typeof e === "object" && "paths" in e) {
          const { paths, ...options } = e;
          stream.PatchSignals(
            JSON.stringify(paths),
            options || undefined,
          );
        }
        break;
    }
  });
}
