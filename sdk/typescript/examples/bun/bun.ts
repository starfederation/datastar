import { ServerSentEventGenerator } from "datastar-sdk/web";

const server = Bun.serve({
  port: 3000,
  routes: {
    "/": () => {
      return new Response(
        `<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@release-candidate/bundles/datastar.js"></script></head><body><div id="toMerge" data-signals-foo="'World'" data-on-load="@get('/merge')">Hello</div></body></html>`,
        {
          headers: { "Content-Type": "text/html" },
        },
      );
    },
    "/merge": async (req: Request) => {
      const reader = await ServerSentEventGenerator.ReadSignals(req);

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
        stream.mergeFragments(
          `<div id="toMerge">Hello ${reader.signals.foo}</div>`,
        );
      });
    },
  },
  async fetch(req: Request) {
    return new Response(`Path not found: ${req.url}`, {
      headers: { "Content-Type": "text/html" },
    });
  },
});

console.log("Server is running on http://localhost:" + server.port);