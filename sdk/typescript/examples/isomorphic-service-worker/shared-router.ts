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

export function createRouter() {
  const app = new Hono();

  // Add main page route
  app.get("/", (c) => {
    return c.html(`
        <html><head>
        <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.1/bundles/datastar.js"></script>
        <style>
            .offline-notice { display: none; color: red; }
            .offline .offline-notice { display: block; }
            #largeContent { 
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
        <div class="offline-notice">⚠️ You are offline - using service worker</div>
        
        <div class="stats">
            Check the Network tab in DevTools to compare transfer sizes and times.<br>
        </div>
        
        <div id="largeContent">
            <div class="progress">Content will stream here... (0 items received)</div>
            <div id="largeContentItems"></div>
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

        <hr>
        <h3>Original Demo</h3>
        <div id="toMerge" data-on-load="@get('/merge')">Not merged</div>
                
        <script>
            if ('serviceWorker' in navigator) {
                navigator.serviceWorker.register('/service-worker.js')
                    .then(registration => console.log('Service Worker registered:', registration.scope))
                    .catch(error => console.log('Service Worker registration failed:', error));
            }
            
            // Update UI based on online/offline status
            function updateOnlineStatus() {
                document.body.classList.toggle('offline', !navigator.onLine);
            }
            window.addEventListener('online', updateOnlineStatus);
            window.addEventListener('offline', updateOnlineStatus);
            updateOnlineStatus();

            // Update progress counter
            const observer = new MutationObserver((mutations) => {
                const items = document.querySelectorAll('.item').length;
                const progress = document.querySelector('.progress');
                if (progress) {
                    progress.textContent = 'Streaming content..... (' + items + ' items received)';
                }
            });

            observer.observe(document.body, {
                childList: true,
                subtree: true
            });
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

  // Generate all items at once
  const allItemsContent = Array.from({ length: 100 }, (_, i) => itemTemplate(i))
    .join("\n");

  // Combined endpoint for both compressed and uncompressed data
  app.get("/large-data", async (c) => {
    const bulk = c.req.query("bulk") === "true";
    return ServerSentEventGenerator.stream(async (stream) => {
      // Send initial container
      stream.mergeFragments(
        `<div id="largeContentItems">
              <h1> ${bulk ? "Bulk" : "Streaming"} Content (from ${source})</h1>
              <div id="items"></div>
          </div>`,
      );

      if (bulk) {
        stream.mergeFragments(
          allItemsContent,
          {
            selector: "#items",
            mergeMode: "append",
          },
        );
      } else {
        // Stream items one by one
        for (let i = 0; i < 100; i++) {
          stream.mergeFragments(
            itemTemplate(i),
            {
              selector: "#items",
              mergeMode: "append",
            },
          );
          await delay(15);
        }
      }
    });
  });

  app.get("/merge", async (c) => {
    return ServerSentEventGenerator.stream(async (stream) => {
      stream.mergeFragments(`<div id="toMerge">Merged (from ${source})</div>`);
    });
  });

  app.get("/await", async (c) => {
    return ServerSentEventGenerator.stream(async (stream) => {
      stream.mergeFragments('<div id="toMerge">Merged</div>');
      await delay(10000);
      stream.mergeFragments('<div id="toMerge">After 10 seconds</div>');
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
