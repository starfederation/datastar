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
        const networkResponse = await fetch(event.request);
        router.offline = false; // Reset when network is available
        return networkResponse;
      } catch (error) {
        console.log("Network request failed, using service worker response.");
        router.offline = true; // Set when network fails
        return router.fetch(event.request);
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
