## Signals Changed

## Demo

<div
  data-signals="{foo:['bar','baz','boo']}"
  >
  <pre data-text="ctx.signals.JSON()"></pre>
</div>

## Explanation

```html
<div data-signals="{foo:['bar','baz','boo']}">
    <div data-text="ctx.signals.JSON()"></div>
</div>
```

Arrays aren't really a thing when usings namespaced signals. Instead, you can use an object with keys that are the index of the array. This is because the leaves of the object are the signals, and the keys are the paths to the signals. This shows we will convert them but it's not a good idea to use arrays in signals.

*** Note ***

This can cause issues when sending the signals to the server, as the server will not know how to handle the array. It is best to use objects instead of arrays.