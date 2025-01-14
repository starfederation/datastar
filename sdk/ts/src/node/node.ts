import { createServer } from "node:http";
import { ServerSentEventGenerator } from "./serverSentEventGenerator.ts";
import type { Jsonifiable } from "npm:type-fest";

const hostname = "127.0.0.1";
const port = 3000;

const server = createServer(async (req, res) => {
  if (req.url === "/") {
    const headers = new Headers({ "Content-Type": "text/html" });
    res.setHeaders(headers);
    res.end(
      `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.1/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
    );
  } else if (req.url?.includes("/merge")) {
    const reader = await ServerSentEventGenerator.readSignals(req);

    if (reader.success) {
      ServerSentEventGenerator.stream(req, res, (stream) => {
        stream.mergeFragments(
          `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
        );
      });
    } else {
      console.error("Error while reading signals", reader.error);
      res.end("Error while reading signals");
    }
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
      const delaySeconds = 5;
      stream.mergeFragments('<div id="toMerge">Merged</div>');
      await delay(delaySeconds * 1000);
      stream.mergeFragments(
        `<div id="toMerge">After ${delaySeconds} seconds</div>`,
      );
    });
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
          stream.mergeFragments(fragments as string, options || undefined);
        }
        break;
      case "removeFragments":
        if (e !== null && typeof e === "object" && "selector" in e) {
          const { selector, ...options } = e;
          stream.removeFragments(selector as string, options || undefined);
        }
        break;
      case "mergeSignals":
        if (e !== null && typeof e === "object" && "signals" in e) {
          const { signals, ...options } = e;
          stream.mergeSignals(
            signals as Record<string, Jsonifiable>,
            options || undefined,
          );
        }
        break;
      case "removeSignals":
        if (e !== null && typeof e === "object" && "paths" in e) {
          const { paths, ...options } = e;
          stream.removeSignals(paths as string[], options || undefined);
        }
        break;
      case "executeScript":
        if (e !== null && typeof e === "object" && "script" in e) {
          const { script, ...options } = e;
          stream.executeScript(script as string, options || undefined);
        }
        break;
    }
  });
}
