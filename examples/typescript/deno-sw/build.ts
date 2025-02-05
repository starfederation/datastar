import * as esbuild from "https://deno.land/x/esbuild/mod.js";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";

await esbuild.build({
  plugins: [...denoPlugins()],
  write: true,
  entryPoints: ["./src/service-worker.ts"],
  outfile: "./src/static/service-worker.js",
  bundle: true,
  minify: false,
  format: "esm",
  legalComments: "none",
  platform: "browser",
  conditions: ["worker", "browser"],
  resolveExtensions: [".ts", ".js", ".mjs"],
  loader: {
    ".ts": "ts",
  },
});

esbuild.stop();
