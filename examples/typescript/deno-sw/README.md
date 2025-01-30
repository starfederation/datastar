This is a sample app that can serve the same responses from both the Deno backend and the Service Worker while offline. 

To test it, start the server with `deno run -A deno.ts` and load the site at `http://localhost:8000`.

The service worker will install and static assets will be cached. You can then either turn on offline mode in your browser's dev tools Network tab, or turn off the Deno server (there will be a slight delay in the response as it tries to fetch from the network first). Reload the page while offline and it will load from the service worker! And the SSE response when you click Start will be rendered from the SW as well.

If you want to expand upon it, it would be best to modify the `shared-router.ts` file, which is what both versions ultimately use (other than for loading static assets, which require different mechanisms in each environment).

There's many other things that can be done from a service worker - including different caching and network strategies. [Google Workbox](https://developer.chrome.com/docs/workbox) is a good resource - both as an easy-to-use library as well as just reference material for all things PWA.