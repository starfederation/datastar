import { Hono } from "jsr:@hono/hono";
import { ServerSentEventGenerator } from "../../src/web/serverSentEventGenerator.ts";

// Helper to detect if we're in a service worker context
const isServiceWorker = () => {
  try {
    return self instanceof ServiceWorkerGlobalScope;
  } catch {
    return false;
  }
};

const source = isServiceWorker() ? "Service Worker" : "Deno Server";

type OfflineState = { value: boolean };

export function createRouter(offline?: OfflineState) {
  const app = new Hono();

  // Add main page route
  app.get("/", (c) => {
    return c.html(`
        <html><head>
        <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.1/bundles/datastar.js"></script>
        <style>
            .offlineNotice { display: block; color: red;}
            #ds-content { 
                height: 450px; 
                overflow: auto; 
                border: 1px solid #ccc; 
            }
            .stats { margin: 20px 0; padding: 10px; background: #f0f0f0; }
            .progress {
                position: sticky;
                top: 0;
                background: #fff;
                padding: 10px;
                border-bottom: 1px solid #ccc;
            }
            .progress-bar {
                width: 100%;
                background: #e0e0e0;
                border-radius: 5px;
                overflow: hidden;
            }
            .progress-bar-fill {
                height: 20px;
                background: #76c7c0;
                width: 0;
                transition: width 0.25s;
                color: green;
            }
            .item {
                padding: 10px;
                margin: 5px 0;
                border: 1px solid #eee;
                animation: fadeIn 0.5s ease-in;
            }
            @keyframes fadeIn {
                from { opacity: 0; }
                to { opacity: 1; }
            }
        </style>
        </head><body>
        
        <div class="stats">
        Check the Network tab in DevTools to compare transfer sizes and times.<br>
        </div>
        
        
        <div class="offlineNotice" data-on-load="$offlineSig=false" data-show="$offlineSig">⚠️ You are offline - using service worker</div>
        <div class="progress">
                <div class="progress-bar">
                    <div id="progress-fill" class="progress-bar-fill" ds-attr-style=$width></div>
                </div>
                <div style="margin-top: 5px; font-size: 0.9em;">
                    Items: <span data-text="$numItems"></span>
                </div>
            </div>
        <div id="ds-content">
          
          
        </div>
        <div class="compression-options">
            <h4>Streaming Data</h4>
                        
            <button data-on-click="@get('/large-data')">
                Streamed Fragments
            </button>

            <h4>Bulk Data</h4>
            <button data-on-click="@get('/large-data?bulk=true')">
                Bulk Fragment
            </button>
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
  const itemTemplate = (index: number) => `
  <div class="item">
      <h2>Sample Content #${index + 1}</h2>
      <p>This is a longer piece of content that will be repeated many times to demonstrate compression effectiveness.</p>
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
  </div>
`;

  // Combined endpoint for both compressed and uncompressed data
  app.get("/large-data", async (c) => {
    const bulk = c.req.query("bulk") === "true";
    return ServerSentEventGenerator.stream(async (stream) => {
      stream.mergeSignals(
        { offlineSig: offline?.value },
      );
      // set number of items to random between 50 and 100
      const numItems = Math.floor(Math.random() * 50) + 50;

      stream.mergeFragments(
        `<div id="ds-content">
          <h1>${
          bulk ? "Bulk" : "Streaming"
        } Content (from ${source})</h1>            
            <div id="items"></div>
        </div>`,
      );

      if (bulk) {
        // Generate all items at once
        const allItemsContent = Array.from(
          { length: numItems },
          (_, i) => itemTemplate(i),
        )
          .join("\n");

        stream.mergeFragments(
          allItemsContent,
          {
            selector: "#items",
            mergeMode: "append",
          },
        );
        stream.mergeFragments(
          `<div id="progress-fill" class="progress-bar-fill" style="width: 100%"></div>`,
        );
        stream.mergeSignals(
          { numItems: numItems, width: `width: 100%` },
        );
      } else {
        // Stream items one by one
        for (let i = 0; i < numItems; i++) {
          stream.mergeFragments(
            itemTemplate(i),
            {
              selector: "#items",
              mergeMode: "append",
            },
          );
          const progress = ((i + 1) / numItems) * 100;
          stream.mergeFragments(
            `<div id="progress-fill" class="progress-bar-fill" style="width: ${progress}%"></div>`,
          );
          stream.mergeSignals(
            { numItems: i + 1, width: `width: ${progress}%` },
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
