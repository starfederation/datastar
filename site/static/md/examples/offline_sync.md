## Offline Sync

## Demo

<div
     data-signals='{"existingSignals":"this will persist in the signals even without network"}'
     data-persist
     data-on-online__window="console.log('online, syncing'); @put('/examples/offline_sync/sync')"
     data-on-offline__window="console.log('offline')"
>
     <div id="results">Go offline, then online to see the signals sync</div>
</div>

## Explanation

```html
<div
  data-signals='{"existingSignals":"this will persist in the signals even without network"}'
  data-persist
  data-on-online__window="console.log('online, syncing'); @put('/examples/offline_sync/sync')"
  data-on-offline__window="console.log('offline')"
>
  <div id="results"></div>
</div>
```

The `data-persist` attribute dumps the signals to a `datastar` key in localStorage. A similar thing happens with the `__session` modifier but gets saved to sessionStorage. Any updates will be saved and reload on page refresh. To fully work this needs a service worker like [workbox](https://developers.google.com/web/tools/workbox/) to be installed. To test it go to the network tab of your browser's dev tools and toggle Online/Offline.
