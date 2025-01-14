import { serve } from "https://deno.land/std@0.140.0/http/server.ts";
import { ServerSentEventGenerator } from "./serverSentEventGenerator.ts";
import { Jsonifiable } from "type-fest";

serve(async (req: Request) => {
  const url = new URL(req.url);

  if (url.pathname === "/") {
    return new Response(
      `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head><body><div id="toMerge" data-on-load="sse('/merge', {method: 'get'})">Not merged</div></body></html>`,
      {
        headers: { "Content-Type": "text/html" },
      },
    );
  } else if (url.pathname.includes("/merge")) {
    return ServerSentEventGenerator.stream((stream) => {
      stream.mergeFragments('<div id="toMerge">Merged</div>');
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
      stream.mergeFragments('<div id="toMerge">Merged</div>');
      await delay(10000);
      stream.mergeFragments('<div id="toMerge">After 10 seconds</div>');
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
