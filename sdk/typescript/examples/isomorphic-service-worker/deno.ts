import { Hono } from "jsr:@hono/hono/tiny";
import { serveStatic } from "jsr:@hono/hono/deno";
import { createRouter } from "./shared-router.ts";

// Rebuild service worker first
const minify = Deno.env.get("MINIFY") === "true";
const buildProcess = new Deno.Command(Deno.execPath(), {
  args: ["run", "-A", "--unstable-sloppy-imports", "build.js"],
  env: { MINIFY: minify ? "true" : "false" },
});
await buildProcess.output();

// Now start the server
const app = new Hono();
const swPath = minify ? "/service-worker.min.js" : "/service-worker.js";

// Serve static files from public directory
app.use("/*", serveStatic({ root: "./public" }));

// Simple direct path serving for service worker
app.get(swPath, serveStatic({ path: `./public${swPath}` }));

// Mount the shared router with the correct service worker path
app.route("/", createRouter({ serviceWorkerPath: swPath }));

Deno.serve({
  port: 8000,
  hostname: "localhost", // Changed to only listen on localhost
  onListen({ port }) {
    console.log(`Deno server running on http://localhost:${port}`);
  },
}, app.fetch);
