import { Hono } from "jsr:@hono/hono";
import { html as honoHtml } from "jsr:@hono/hono/html";
import { ServerSentEventGenerator } from "../../src/web/serverSentEventGenerator.ts";

// Extend Hono type with our offline property
interface RouterApp extends Hono {
  offline: boolean; // Simpler - offline is now just a boolean
}

// Wrap Hono's html to automatically convert to string, which is required by d* mergeFragments, handling both sync and async cases
// Hono html function is used because it can escape values and also allows for syntax highlighting in your IDE if you have an appropriate extension installed (e.g. https://marketplace.visualstudio.com/items?itemName=runem.lit-plugin)
const html = (strings: TemplateStringsArray, ...values: unknown[]) => {
  return honoHtml(strings, ...values).toString();
};

// Helper to detect if we're in a service worker context
const isServiceWorker = () => {
  try {
    return self instanceof ServiceWorkerGlobalScope;
  } catch {
    return false;
  }
};

const source = isServiceWorker() ? "Service Worker" : "Deno Server";

// Move offline state inside the router
export function createRouter() {
  const app = new Hono() as RouterApp;
  app.offline = false; // Simpler assignment

  // Add main page route
  app.get("/", (c) => {
    return c.html(html`
        <html><head>
        <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.1/bundles/datastar.js"></script>
        <link rel="stylesheet" href="/styles.css">
        </head><body>
        
        <div
          class="offlineNotice"
          data-signals="{offlineSig:false}"
          data-show="$offlineSig"
          data-on-offline__window="$offlineSig=true"
          data-on-online__window="$offlineSig=false"
        >⚠️ You are offline - using service worker</div>

        <div class="progress">
                <div class="progress-bar">
                    <div id="progress-fill" class="progress-bar-fill" data-attr-style="$width"></div>
                </div>
                <div class="items-counter">
                    Items: <span data-signals="{itemsReceived:0}"
                    data-text="$itemsReceived"></span>
                </div>
            </div>
        <div id="ds-content">
          
        </div>
        <div class="demo-controls">
            <h2>Isomorphic Server/Service Worker Demo</h2>
            <div class="description">
                <p>This demo shows how the same routing and rendering code runs in both:</p>
                <ul>
                    <li><strong>Deno Server</strong> - When online, content is served from the backend</li>
                    <li><strong>Service Worker</strong> - When offline, the same code generates content locally</li>
                </ul>
            </div>
            
            <div class="button-group">
                <button data-on-click="@get('/large-data')">
                    Streamed Content
                </button>

                <button data-on-click="@get('/large-data?bulk=true')">
                    Bulk Content
                </button>
            </div>
                        
            <h3>Content Delivery Modes:</h3>
            <div class="description">
              <ul >
                  <li><strong>Streaming Mode:</strong> Content streams in chunks, updating progressively</li>
                  <li><strong>Bulk Mode:</strong> All content arrives in a single response</li>
              </ul>
              <p>Inspect each mode's transfer size and speed in your browser Dev Tools' network tab. <br> You'll notice an even larger difference if you implement a compression middleware (either in your Deno app or in a reverse proxy like Caddy) </p>
            </div>
        </div>

        <script>
            if ('serviceWorker' in navigator) {
                navigator.serviceWorker.register('/service-worker.js')
                    .then(registration => console.log('Service Worker registered:', registration.scope))
                    .catch(error => console.log('Service Worker registration failed:', error));
            }
        </script>
        </body></html>
    `);
  });

  // Single item template
  const itemTemplate = (index: number) =>
    html`
  <div class="item">
      <h2>Sample Content #${index + 1}</h2>
      <p>This is a longer piece of content that will be repeated many times to demonstrate compression effectiveness.</p>
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
  </div>
`;

  app.get("/large-data", async (c) => {
    const bulk = c.req.query("bulk") === "true";
    return ServerSentEventGenerator.stream(async (stream) => {
      stream.mergeSignals({ offlineSig: app.offline });
      const numItems = Math.floor(Math.random() * 50) + 50;

      stream.mergeFragments(
        html`<div id="ds-content">
          <h1>${bulk ? "Bulk" : "Streaming"} Content (from ${source})</h1>
          <div id="items"></div>
        </div>`,
      );

      if (bulk) {
        stream.mergeFragments(
          Array.from({ length: numItems }, (_, i) => itemTemplate(i)).join(
            "\n",
          ),
          {
            selector: "#items",
            mergeMode: "append",
          },
        );

        stream.mergeSignals(
          { itemsReceived: numItems, width: `width: 100%` },
        );
      } else {
        for (let i = 0; i < numItems; i++) {
          stream.mergeFragments(
            itemTemplate(i),
            {
              selector: "#items",
              mergeMode: "append",
            },
          );
          const progress = ((i + 1) / numItems) * 100;
          stream.mergeSignals(
            { itemsReceived: i + 1, width: `width: ${progress}%` },
          );
          await delay(10);
        }
      }
    });
  });

  // Catch-all route with context-aware handling
  app.all("*", async (c) => {
    if (isServiceWorker()) {
      // In service worker, pass through to network
      const response = await fetch(c.req.raw);
      return response;
    }
    // In Deno server, return 404
    return c.text(`Path not found: ${c.req.url}`, 404);
  });

  return app;
}

function delay(milliseconds: number) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}
