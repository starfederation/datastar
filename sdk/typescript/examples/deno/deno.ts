import { serve } from "https://deno.land/std@0.140.0/http/server.ts";
import { ServerSentEventGenerator } from "npm:datastar-sdk/web";

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
    const reader = await ServerSentEventGenerator.readSignals(req);

    if (!reader.success) {
      console.error("Error while reading signals", reader.error);

      return new Response(`Error while reading signals`, {
        headers: { "Content-Type": "text/html" },
      });
    }

    if (!("foo" in reader.signals)) {
      console.error("The foo signal is not present");

      return new Response("The foo signal is not present", {
        headers: { "Content-Type": "text/html" },
      });
    }

    return ServerSentEventGenerator.stream((stream) => {
      stream.patchElements(
        `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
      );
    });
  }

  return new Response(`Path not found: ${req.url}`, {
    headers: { "Content-Type": "text/html" },
  });
});
