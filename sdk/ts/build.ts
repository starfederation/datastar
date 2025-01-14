// ex. scripts/build_npm.ts
import { build, emptyDir } from "@deno/dnt";

await emptyDir("./npm");

await build({
  entryPoints: ["./src/node/node.ts", "./src/web/serverSentEventGenerator.ts"],
  outDir: "./npm",
  shims: {
    // see JS docs for overview and more options
    deno: true
  },
  package: {
    // package.json properties
    name: "datastar-sdk",
    version: Deno.args[0],
    description: "Cross-runtime Javascript SDK for Datastar",
    license: "MIT",
    repository: {
      type: "git",
      url: "git+https://github.com/starfederation/datastar.git",
    },
    bugs: {
      url: "https://github.com/starfederation/datastar/issues",
    },
  },
  postBuild() {
    // steps to run after building and before running the tests
    //Deno.copyFileSync("LICENSE", "npm/LICENSE");
    Deno.copyFileSync("README.md", "npm/README.md");
  },
  rootTestDir: './src',
});