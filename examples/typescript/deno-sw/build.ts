import * as esbuild from "https://deno.land/x/esbuild/mod.js";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";

// Build with configuration based on MINIFY env var
await esbuild.build({
  plugins: [...denoPlugins()],
  write: true,
  entryPoints: ["./service-worker.ts"],
  outfile: "./static/service-worker.js",
  bundle: true,
  minify: false,
  format: "esm",
  legalComments: "none",
  platform: "browser",
  conditions: ["worker", "browser"],
  // Add TypeScript resolution
  resolveExtensions: [".ts", ".js", ".mjs"],
  loader: {
    ".ts": "ts",
  },
});

esbuild.stop();
