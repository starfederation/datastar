import { Hono } from "jsr:@hono/hono/tiny";
import { ServerSentEventGenerator } from "../../../sdk/typescript/src/web/serverSentEventGenerator.ts";
import { getHelloWorldHtml } from "./hello-world.js";

interface Store {
  delay: number;
}

export function createRouter() {
  const app = new Hono();
    
  // Homepage route
  app.get("/", async (c) => {
    console.log("Handling / route");
    return c.html(getHelloWorldHtml());
  });
  
  // Hello, world! route
  app.get("/hello-world", async (c) => {
    const reader = await ServerSentEventGenerator.readSignals<Store>(c.req);

    if (!reader.success) {
      return c.text(`Error while reading signals: ${reader.error}`, 400);
    }

    return ServerSentEventGenerator.stream(async (stream) => {
      const message = "Hello, world!";
      
      for (let i = 0; i < message.length; i++) {
        stream.mergeFragments(
          `<div id="message">${message.substring(0, i + 1)}</div>`,
        );
        await new Promise(resolve => setTimeout(resolve, reader.signals.delay));
      }
    });
  });
  
  // Catch-all route - just return 404 for unmatched routes
  app.all("*", (c) => {
    console.log(`Route not found: ${c.req.url}`);
    return c.text(`Path not found: ${c.req.url}`, 404);
  });

  return app;
}

