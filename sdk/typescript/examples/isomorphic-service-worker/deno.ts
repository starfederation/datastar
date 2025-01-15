import { Hono } from "jsr:@hono/hono";
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
const swPath = minify
  ? "./public/service-worker.min.js"
  : "./public/service-worker.js";

// Serve the correct version of the service worker
app.get("/service-worker.js", serveStatic({ path: swPath }));

// Mount the shared router
app.route("/", createRouter());

Deno.serve(app.fetch);
