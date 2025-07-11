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
      stream.patchElements(
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
      stream.patchElements('<div id="toMerge">Merged</div>');
      await delay(5000);
      stream.patchElements('<div id="toMerge">After 10 seconds</div>');
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
    
    // Convert camelCase to method calls like Python SDK does
    switch (type) {
      case "patchElements":
        handlepatchElements(stream, e);
        break;
      case "removeElements":
        handleRemoveElements(stream, e);
        break;
      case "patchSignals":
        handlepatchSignals(stream, e);
        break;
      case "removeSignals":
        handleRemoveSignals(stream, e);
        break;
      case "executeScript":
        handleExecuteScript(stream, e);
        break;
      // Legacy support for old event types
      case "mergeFragments":
        handlepatchElements(stream, { ...e, mode: e.mode || "outer" });
        break;
      case "removeFragments":
        handleRemoveElements(stream, e);
        break;
      case "mergeSignals":
        handlepatchSignals(stream, e);
        break;
    }
  });
}

function handlepatchElements(stream: ServerSentEventGenerator, e: Record<string, Jsonifiable>) {
  if (e !== null && typeof e === "object") {
    const { elements, mode, selector, useViewTransition, ...options } = e;
    
    // Build patch options
    const patchOptions: Record<string, unknown> = { ...options };
    if (mode && mode !== "outer") patchOptions.mode = mode;
    if (selector) patchOptions.selector = selector;
    if (useViewTransition !== undefined) patchOptions.useViewTransition = useViewTransition;
    
    // For remove mode, elements might be empty which is fine
    const elementsToUse = (elements as string) || "";
    stream.patchElements(elementsToUse, patchOptions);
  }
}

function handleRemoveElements(stream: ServerSentEventGenerator, e: Record<string, Jsonifiable>) {
  if (e !== null && typeof e === "object" && "selector" in e) {
    const { selector, ...options } = e;
    stream.patchElements("", { ...options, mode: "remove", selector: selector as string });
  }
}

function handlepatchSignals(stream: ServerSentEventGenerator, e: Record<string, Jsonifiable>) {
  if (e !== null && typeof e === "object") {
    const { signals, "signals-raw": signalsRaw, ...options } = e;
    
    if (signalsRaw) {
      stream.patchSignals(signalsRaw as string, options || undefined);
    } else if (signals) {
      stream.patchSignals(JSON.stringify(signals), options || undefined);
    }
  }
}

function handleRemoveSignals(stream: ServerSentEventGenerator, e: Record<string, Jsonifiable>) {
  if (e !== null && typeof e === "object" && "paths" in e) {
    const { paths, ...options } = e;
    const pathArray = paths as string[];
    const removeSignals: Record<string, null> = {};
    pathArray.forEach(path => {
      removeSignals[path] = null;
    });
    stream.patchSignals(JSON.stringify(removeSignals), options || undefined);
  }
}

function handleExecuteScript(stream: ServerSentEventGenerator, e: Record<string, Jsonifiable>) {
  if (e !== null && typeof e === "object" && "script" in e) {
    const { script, autoRemove = true, attributes, ...options } = e;
    let scriptElement = `<script`;
    
    // Add auto-remove behavior first (Python SDK pattern)
    if (autoRemove) {
      scriptElement += ` data-effect="el.remove()"`;
    }
    
    // Add attributes if provided
    if (attributes && typeof attributes === "object") {
      for (const [key, value] of Object.entries(attributes)) {
        scriptElement += ` ${key}="${value}"`;
      }
    }
    
    scriptElement += `>${script}</script>`;
    
    // Use append mode with body selector (Python SDK pattern)
    stream.patchElements(scriptElement, { 
      mode: "append", 
      selector: "body",
      ...options 
    });
  }
}
