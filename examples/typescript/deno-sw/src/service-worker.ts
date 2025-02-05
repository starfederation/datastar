import { createRouter } from "./shared-router.ts";

declare const self: ServiceWorkerGlobalScope;

const CACHE_NAME = 'datastar-cache';
const CORE_ASSETS = [
  '/',
  'https://unpkg.com/@tailwindcss/browser@4',
  'https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.2/bundles/datastar.js',
  'https://data-star.dev/static/images/rocket.png'
];

const router = createRouter();

self.addEventListener('install', (event) => {
  console.log('SW Install');
  event.waitUntil(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      for (const url of CORE_ASSETS) {
        try {
          const fetchOptions = url.includes('rocket') ? { mode: 'no-cors' as RequestMode } : undefined;
          const response = await fetch(url, fetchOptions);
          await cache.put(url, response);
          console.log('Cached:', url);
        } catch (error) {
          console.error('Failed to cache:', url, error);
        }
      }
      await self.skipWaiting();
    })()
  );
});

self.addEventListener('activate', (event) => {
  console.log('SW Activate');
  event.waitUntil(self.clients.claim());
});

self.addEventListener('fetch', (event) => {
  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE_NAME);
      const url = new URL(event.request.url);
     
      // Try cache first for core assets
      if (CORE_ASSETS.includes(url.toString()) || CORE_ASSETS.includes(url.pathname)) {
        const cachedResponse = await cache.match(event.request);
        if (cachedResponse) {
          console.log('SW Cache Hit:', event.request.url);
          return cachedResponse;
        }
      }

      // If browser is set to offline, use router for dynamic routes
      if (!self.navigator.onLine) {
        console.log('Browser is offline, using router fallback');
        return await router.fetch(event.request);
      }

      // Try network
      try {
        console.log('SW Fetch:', event.request.url);
        
        const fetchOptions = event.request.url.includes('rocket') ? { mode: 'no-cors' as RequestMode } : undefined;
        
        const networkResponse = await fetch(event.request, fetchOptions);
        
        // Cache successful GET requests for core assets
        if (networkResponse.ok &&
          event.request.method === 'GET'
          && (CORE_ASSETS.includes(url.toString()) || CORE_ASSETS.includes(url.pathname))
        )
        {
          await cache.put(event.request, networkResponse.clone());
          console.log('SW Cache Put:', event.request.url);
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
