import { createRouter } from "./shared-router.ts";

declare const self: ServiceWorkerGlobalScope;

const CACHE_NAME = 'datastar-cache-v1';
const CORE_ASSETS = [
  '/',
  '/static/tailwind.js',
  '/static/datastar.js',
  '/static/datastar.js.map',
  '/static/rocket.png',
  '/service-worker.js'
];

const router = createRouter();

self.addEventListener('install', (event) => {
  console.log('SW Install');
  event.waitUntil(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      await Promise.all(
        CORE_ASSETS.map(url => fetch(url).then(response => cache.put(url, response)))
      );
      await self.skipWaiting();
    })()
  );
});

self.addEventListener('activate', (event) => {
  console.log('SW Activate');
  event.waitUntil(self.clients.claim());
});

self.addEventListener('fetch', (event) => {
  console.log('SW Fetch:', event.request.url);
  
  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      const url = new URL(event.request.url);

      // Try cache first for core assets
      if (CORE_ASSETS.includes(url.pathname)) {
        const cachedResponse = await cache.match(event.request);
        if (cachedResponse) {
          return cachedResponse;
        }
      }

      // If offline, use router for dynamic routes
      if (!self.navigator.onLine) {
        console.log('Browser is offline, using router fallback');
        return await router.fetch(event.request);
      }

      // Try network
      try {
        const networkResponse = await fetch(event.request);
        
        // Cache successful GET requests for core assets
        if (networkResponse.ok &&
            event.request.method === 'GET' &&
            CORE_ASSETS.includes(url.pathname)) {
          await cache.put(event.request, networkResponse.clone());
        }
        
        return networkResponse;
      } catch (error) {
        // Fall back to router for dynamic routes
        console.log('Network request failed, using router fallback');
        return await router.fetch(event.request);
      }
    })()
  );
});
