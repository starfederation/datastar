import * as esbuild from "https://deno.land/x/esbuild/mod.js";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";
import { minifyTemplates, writeFiles } from 'npm:esbuild-minify-templates';

const minify = Deno.env.get("MINIFY") === "true";
const outfile = minify
  ? "./public/service-worker.min.js"
  : "./public/service-worker.js";

// Build with configuration based on MINIFY env var
await esbuild.build({
  plugins: [
    minifyTemplates(), writeFiles(),
    ...denoPlugins()
  ],
  write: false,
  entryPoints: ["./service-worker.ts"],
  outfile,
  bundle: true,
  minify,
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
