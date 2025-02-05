import { Hono } from "jsr:@hono/hono/tiny";
import { serveStatic } from "jsr:@hono/hono/deno";
import { createRouter } from "./shared-router.ts";

const app = new Hono();

// Serve static files
app.use("/static/*", serveStatic({ root: "./src" }));

// Serve the service worker from the root path - this is need so that it has scope for the entire application rather than just /static
app.use("/service-worker.js", serveStatic({ path: "./src/static/service-worker.js" }));

// Mount the shared router at the root path
app.route("/", createRouter());

Deno.serve({
  port: 8000,
  hostname: "127.0.0.1",
}, app.fetch);
