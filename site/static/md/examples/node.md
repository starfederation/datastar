## Explanation

An example backend in Node.

```js
const express = require("express");
const { randomBytes } = require("crypto");

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: true }));
const backendData = {};

function indexPage() {
  const indexPage = `
    <!doctype html><html>
      <head>
        <title>Node/Express + Datastar Example</title>
        <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head>
      <body>
        <h2>Node/Express + Datastar Example</h2>
        <main class="container" id="main" data-signals='{ input: "", show: false }'>
        <input type="text" placeholder="Type here!" data-bind="input" />
        <button data-on-click="@sse('/put', {method:'put'})">Send State</button>
        <div id="output"></div>
        <button data-on-click="@sse('/get')">Get Backend State</button>
        <div id="output2"></div>
        <button data-on-click="$show=!$show">Toggle</button>
        <div data-show="$show">
          <span>Hello From Datastar!</span>
        </div>
        <div>
          <span>Feed from server: </span>
          <span id="feed" data-on-load="@sse('/feed')"></span>
        </div>
      </body>
    </html>`;
  return indexPage;
}

app.get("/", (req, res) => {
  res.send(indexPage()).end();
});

function setHeaders(res) {
  res.set({
    "Cache-Control": "no-cache",
    "Content-Type": "text/event-stream",
    Connection: "keep-alive",
  });
  res.flushHeaders();
}

function sendSSE({ res, frag, selector, merge, mergeType, end }) {
  res.write("event: datastar-merge-fragments\n");
  if (selector) res.write(`data: selector {$selector}\n`);
  if (merge) res.write(`data: mergeMode {$mergeType}\n`);
  res.write(`data: fragments {$frag}\n\n`);
  if (end) res.end();
}

app.put("/put", (req, res) => {
  setHeaders(res);
  const { input } = req.body;
  backendData.input = input;
  const output = `Your input: {$input}, is {$input.length} long.`;
  let frag = `<div id="output">{$output}</div>`;
  sendSSE({
    res,
    frag,
    selector: null,
    merge: true,
    mergeType: "morph",
    end: true,
  });
});

app.get("/get", (req, res) => {
  setHeaders(res);

  const output = `Backend State: {$JSON.stringify(backendData)}.`;
  let frag = `<div id="output2">{$output}</div>`;

  sendSSE({
    res,
    frag,
    selector: null,
    merge: true,
    mergeType: "morph",
    end: false,
  });
  frag = `<div id="output3">Check this out!</div>;`;
  sendSSE({
    res,
    frag,
    selector: "#main",
    merge: true,
    mergeType: "prepend",
    end: true,
  });
});

app.get("/feed", async (req, res) => {
  setHeaders(res);
  while (res.writable) {
    const rand = randomBytes(8).toString("hex");
    const frag = `<span id="feed">{$rand}</span>`;
    sendSSE({
      res,
      frag,
      selector: null,
      merge: false,
      mergeType: null,
      end: false,
    });
    await new Promise((resolve) => setTimeout(resolve, 1000));
  }
  res.end();
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Server is running on http://localhost:{$PORT}`);
});
```
