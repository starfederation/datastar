import { createRouter } from "./shared-router.ts";

declare const self: ServiceWorkerGlobalScope;

const router = createRouter();

self.addEventListener("fetch", (event) => {
  if (event.request.url.includes("/service-worker.js")) {
    return;
  }

  event.respondWith(
    (async () => {
      try {
        // Try network first
        const networkResponse = await fetch(event.request);
        return networkResponse;
      } catch (error) {
        // If network fails, handle with service worker
        console.log("Network request failed, using service worker response.");
        return router.fetch(event.request, {
          headers: event.request.headers,
        });
      }
    })(),
  );
});

self.addEventListener("install", (event) => {
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});
