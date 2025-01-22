import { createRouter } from "./shared-router.ts";

declare const self: ServiceWorkerGlobalScope;

const STATIC_EXTENSIONS = [
  ".css",
  ".js",
];

const isStaticFile = (url: string) => {
  return STATIC_EXTENSIONS.some((ext) => url.endsWith(ext));
};

const router = createRouter();

self.addEventListener("fetch", (event) => {
  if (event.request.url.includes("service-worker")) {
    return;
  }

  event.respondWith(
    (async () => {
      const url = new URL(event.request.url);

      // First check cache for static files
      if (isStaticFile(url.pathname)) {
        const cache = await caches.open("static-v1");
        const cachedResponse = await cache.match(url.pathname);
        if (cachedResponse) {
          return cachedResponse;
        }
      }

      try {
        const networkResponse = await fetch(event.request);
        router.offline = false;

        // Cache successful static file responses
        if (networkResponse.ok && isStaticFile(url.pathname)) {
          const cache = await caches.open("static-v1");
          await cache.put(url.pathname, networkResponse.clone());
        }

        return networkResponse;
      } catch (error) {
        console.log("Network request failed, using service worker response.");
        router.offline = true;

        // Get response from router
        const response = await router.fetch(event.request);

        // If this is an SSE endpoint, preserve the streaming nature
        if (
          response.headers.get("Content-Type")?.includes("text/event-stream")
        ) {
          return new Response(response.body, {
            headers: {
              "Content-Type": "text/event-stream",
              "Cache-Control": "no-cache",
              "Connection": "keep-alive",
            },
          });
        }

        return response;
      }
    })(),
  );
});

self.addEventListener("install", (event) => {
  event.waitUntil(
    (async () => {
      const cache = await caches.open("static-v1");
      await cache.add("/styles.css");
      self.skipWaiting();
    })(),
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});
