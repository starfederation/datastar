## Session Storage

## Demo

<div data-signals="{sessionId:1234, count:0}" data-persist__session data-on-raf="$count++">Look at your DevTools session
  storage!</div>

## Explanation

```html
<div data-signals="{sessionId:1234, count:0}" data-persist__session data-on-raf="$count++"></div>
```

A community user wanted the ability to signals data in the session storage. This is now possible with the
`data-persist__session` attribute. The data will be saved to the session storage and will be available even after a page
refresh. The data will be removed when the session ends.

You'll have to go into the browser's dev tools to see the data in the session storage. The data will be saved in the
`datastar` key. In this example, the session storage will have the key `sessionId` be `1234` and the key `count` start
at `0`. The `data-on-raf` attribute is used to increment the `count` key every time a requestAnimationFrame is called.