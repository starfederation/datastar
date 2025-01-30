import { Hono } from "jsr:@hono/hono";
import { serveStatic } from "jsr:@hono/hono/deno";
import { createRouter } from "./shared-router.ts";

// Rebuild service worker first
const buildProcess = new Deno.Command(Deno.execPath(), {
  args: ["run", "-A", "--unstable-sloppy-imports", "build.js"],
});
await buildProcess.output();

const app = new Hono();

// Middleware to log incoming requests
app.use("*", async (c, next) => {
  console.log(`Incoming request: ${c.req.method} ${c.req.url}`);
  await next();
});


// Serve static files from public directory after router
app.use("/static/*", serveStatic({ root: "./" }));

// Serve the service worker from the root path
app.use("/service-worker.js", serveStatic({ path: "./static/service-worker.js" }));

// Mount the shared router at the root path
app.route("/", createRouter());


// Serve the fallback static file
// app.use("/", serveStatic({ path: "./public/hello-world.html" }));

Deno.serve({
  port: 8000,
  hostname: "127.0.0.1",
}, app.fetch);
