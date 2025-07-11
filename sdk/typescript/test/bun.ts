import { ServerSentEventGenerator } from "../src/web/serverSentEventGenerator";

// This server is used for testing the Bun web standard based sdk
const server = Bun.serve({
  port: 8001,
  async fetch(req) {
    const url = new URL(req.url);

    if (url.pathname === "/") {
      return new Response(
        `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@main/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
        {
          headers: { "Content-Type": "text/html" },
        },
      );
      } else if (url.pathname.includes("/merge")) {
    const reader = await ServerSentEventGenerator.readSignals(req);

      if (!reader.success) {
        console.error("Error while reading signals", reader.error);
        return new Response(`Error while reading signals`);
      }

      if (!("foo" in reader.signals)) {
        console.error("The foo signal is not present");
        return new Response("The foo signal is not present");
      }

      return ServerSentEventGenerator.stream((stream) => {
        stream.patchElements(
          `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
        );
      });
      } else if (url.pathname.includes("/test")) {
    const reader = await ServerSentEventGenerator.readSignals(req);
      if (reader.success === true) {
        const events = reader.signals.events;
        if (isEventArray(events)) {
          return ServerSentEventGenerator.stream((stream) => {
            testEvents(stream, events);
          });
        }
      }
    } else if (url.pathname.includes("await")) {
      return ServerSentEventGenerator.stream(async (stream) => {
        stream.patchElements('<div id="toMerge">Merged</div>');
        await delay(5000);
        stream.patchElements('<div id="toMerge">After 5 seconds</div>');
      });
    }

    return new Response(`Path not found: ${req.url}`, {
      headers: { "Content-Type": "text/html" },
    });
  },
});

console.log(`Bun server running at http://localhost:${server.port}/`);

function delay(milliseconds) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

function isEventArray(events) {
  return events instanceof Array && events.every((event) => {
    return typeof event === "object" && event !== null &&
      typeof event.type === "string";
  });
}

function testEvents(stream, events) {
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

function handlepatchElements(stream, e) {
  if (e !== null && typeof e === "object") {
    const { elements, mode, selector, useViewTransition, ...options } = e;
    
    // Build patch options
    const patchOptions = { ...options };
    if (mode && mode !== "outer") patchOptions.mode = mode;
    if (selector) patchOptions.selector = selector;
    if (useViewTransition !== undefined) patchOptions.useViewTransition = useViewTransition;
    
    // For remove mode, elements might be empty which is fine
    const elementsToUse = elements || "";
    stream.patchElements(elementsToUse, patchOptions);
  }
}

function handleRemoveElements(stream, e) {
  if (e !== null && typeof e === "object" && "selector" in e) {
    const { selector, ...options } = e;
    stream.patchElements("", { ...options, mode: "remove", selector: selector });
  }
}

function handlepatchSignals(stream, e) {
  if (e !== null && typeof e === "object") {
    const { signals, "signals-raw": signalsRaw, ...options } = e;
    
    if (signalsRaw) {
      stream.patchSignals(signalsRaw, options || undefined);
    } else if (signals) {
      stream.patchSignals(JSON.stringify(signals), options || undefined);
    }
  }
}

function handleRemoveSignals(stream, e) {
  if (e !== null && typeof e === "object" && "paths" in e) {
    const { paths, ...options } = e;
    const pathArray = paths;
    const removeSignals = {};
    pathArray.forEach(path => {
      removeSignals[path] = null;
    });
    stream.patchSignals(JSON.stringify(removeSignals), options || undefined);
  }
}

function handleExecuteScript(stream, e) {
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