import { Hono } from "jsr:@hono/hono/tiny";
import { ServerSentEventGenerator } from "../../../../sdk/typescript/src/web/serverSentEventGenerator.ts";
import { getHelloWorldHtml } from "./hello-world.js";

// Make Store properties explicitly typed
interface Store {
  delay: number | null;
}

export function createRouter() {
  const app = new Hono();
    
  // Homepage route
  app.get("/", async (c) => {
    return c.html(getHelloWorldHtml());
  });
  
  // Hello, world! route
  app.get("/hello-world", async (c) => {
    const reader = await ServerSentEventGenerator.readSignals(c.req.raw);

    if (!reader.success) {
      return c.text(`Error while reading signals: ${reader.error}`, 400);
    }

    return ServerSentEventGenerator.stream(async (stream) => {
      const message = "Hello, world!";
      const delay = typeof reader.signals.delay === 'number' ? reader.signals.delay : 400; // Default delay
      
      for (let i = 0; i < message.length; i++) {
        stream.mergeFragments(
          `<div id="message">${message.substring(0, i + 1)}</div>`,
        );
        await new Promise(resolve => setTimeout(resolve, delay));
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

