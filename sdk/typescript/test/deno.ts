import { serve } from "https://deno.land/std@0.140.0/http/server.ts";
import { ServerSentEventGenerator } from "../src/web/serverSentEventGenerator.ts";
import type { Jsonifiable } from "../src/types.ts";

// This server is used for testing the web standard based sdk
serve(async (req: Request) => {
  const url = new URL(req.url);

  if (url.pathname === "/") {
    return new Response(
      `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@release-candidate/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
      {
        headers: { "Content-Type": "text/html" },
      },
    );
  } else if (url.pathname.includes("/merge")) {
    const reader = await ServerSentEventGenerator.ReadSignals(req);

    if (!reader.success) {
      console.error("Error while reading signals", reader.error);
      return new Response(`Error while reading signals`);
    }

    if (!("foo" in reader.signals)) {
      console.error("The foo signal is not present");
      return new Response("The foo signal is not present");
    }

    return ServerSentEventGenerator.stream((stream) => {
      stream.PatchElements(
        `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
      );
    });
  } else if (url.pathname.includes("/test")) {
    const reader = await ServerSentEventGenerator.ReadSignals(req);
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
      stream.PatchElements('<div id="toMerge">Merged</div>');
      await delay(5000);
      stream.PatchElements('<div id="toMerge">After 5 seconds</div>');
    });
  }

  return new Response(`Path not found: ${req.url}`, {
    headers: { "Content-Type": "text/html" },
  });
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
          stream.PatchSignals(JSON.stringify(paths), options || undefined);
        }
        break;
    }
  });
}
