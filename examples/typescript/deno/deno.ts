import { ServerSentEventGenerator } from "../../../sdk/typescript/src/web/serverSentEventGenerator.ts";

interface Store {
  delay: number;
}

Deno.serve({
  port: 8000,
  hostname: "localhost",
}, async (req: Request) => {
  const url = new URL(req.url);

  if (url.pathname === "/") {
    const html = await Deno.readFile("./public/hello-world.html");
    return new Response(html, {
      headers: { "Content-Type": "text/html" },
    });
    
  } else if (url.pathname === "/hello-world") {
    const reader = await ServerSentEventGenerator.readSignals<Store>(req);

    if (!reader.success) {
      return new Response(`Error while reading signals: ${reader.error}`);
    }

    const message = "Hello, world!";
    return ServerSentEventGenerator.stream(async (stream) => {
      for (let i = 0; i < message.length; i++) {
        stream.mergeFragments(
          `<div id="message">${message.substring(0, i + 1)}</div>`,
        );
        await new Promise(resolve => setTimeout(resolve, reader.signals.delay));
      }
    });
  }

  return new Response(`Path not found: ${req.url}`, {
    status: 404,
  });
});
