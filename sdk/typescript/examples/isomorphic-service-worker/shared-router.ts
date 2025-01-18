import { Hono } from "jsr:@hono/hono/tiny";
import { html } from "jsr:@hono/hono/html";
import type { HtmlEscapedString } from "jsr:@hono/hono/utils/html"; // Add this import
import { ServerSentEventGenerator } from "../../src/web/serverSentEventGenerator.ts";

// Extend Hono type with our offline property
interface RouterApp extends Hono {
  offline: boolean; // Simpler - offline is now just a boolean
}

// Improved wrapper function with type checking
const mergeFragment = (stream: ServerSentEventGenerator, fragment: unknown, options?: { selector?: string; mergeMode?: "morph" | "inner" | "outer" | "prepend" | "append" | "before" | "after" | "upsertAttributes" }) => {
  let content: string;
  if (typeof fragment === 'object' && fragment !== null) {
    if ('isEscaped' in fragment) {
      // Handle Hono's HtmlEscapedString
      content = (fragment as HtmlEscapedString).toString();
    } else {
      content = String(fragment);
    }
  } else {
    content = String(fragment);
  }
  return stream.mergeFragments(content, options);
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

// Static file extensions that should be cached when offline
const STATIC_EXTENSIONS = [
  ".css",
  ".js",
  ".jpg",
  ".jpeg",
  ".png",
  ".gif",
  ".svg",
  ".woff",
  ".woff2",
];

const isStaticFile = (url: string) => {
  return STATIC_EXTENSIONS.some((ext) => url.endsWith(ext));
};

// Change return type to string since that's what we need
const progressTemplate = (progress: number, itemCount: number) => html`
  <div id="progress-container" class="progress">
    <div class="progress-bar">
        <div id="progress-bar-fill" class="progress-bar-fill" style="width: ${progress}%"></div>
    </div>
    <div class="items-counter">
        Items: <span id="items-count">${itemCount}</span>
    </div>
  </div>
`;

export function createRouter(options: { serviceWorkerPath?: string } = {}) {
  const app = new Hono() as RouterApp;
  app.offline = false; // Simpler assignment

  // Add main page route
  app.get("/", (c) => {
    const swPath = options.serviceWorkerPath || '/service-worker.js';
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
    
      ${progressTemplate(0, 0)}
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
              navigator.serviceWorker.register('${swPath}')
                  .then(registration => console.log('Service Worker registered:', registration.scope))
                  .catch(error => console.log('Service Worker registration failed:', error));
          }
      </script>
      </body></html>
  `)
  });

  // Single item template
  const itemTemplate = (index: number) => html`
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

      mergeFragment(stream, html`<div id="ds-content">
        <h1>${bulk ? "Bulk" : "Streaming"} Content (from ${source})</h1>
        <div id="items"></div>
      </div>`);

      if (bulk) {
        mergeFragment(
          stream,
          Array.from({ length: numItems }, (_, i) => itemTemplate(i)).join("\n"),
          {
            selector: "#items",
            mergeMode: "append",
          }
        );

        mergeFragment(stream, progressTemplate(100, numItems));
      } else {
        for (let i = 0; i < numItems; i++) {
          mergeFragment(
            stream,
            itemTemplate(i),
            {
              selector: "#items",
              mergeMode: "append",
            }
          );
          const progress = ((i + 1) / numItems) * 100;
          
          mergeFragment(stream, progressTemplate(progress, i + 1));
          await delay(10);
        }
      }
    });
  });

  // Catch-all route with context-aware handling
  app.all("*", async (c) => {
    if (isServiceWorker()) {
      try {
        const response = await fetch(c.req.raw);
        // Cache successful static file responses
        const url = new URL(c.req.url);
        if (response.ok && isStaticFile(url.pathname)) {
          const cache = await caches.open("static-v1");
          await cache.put(url.pathname, response.clone());
        }
        return response;
      } catch (error) {
        // Check cache for static files
        const url = new URL(c.req.url);
        if (isStaticFile(url.pathname)) {
          const cache = await caches.open("static-v1");
          const cachedResponse = await cache.match(url.pathname);
          if (cachedResponse) {
            return cachedResponse;
          }
        }
        return c.text("Resource not available offline", 404);
      }
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
