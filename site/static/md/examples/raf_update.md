## Request Animation Frame Update

## Demo

<div data-signals="{currentTime:Date.now()}" data-on-raf="currentTime = Date.now()">
  <div id="time">Current Time: <span data-text="new Date(currentTime).toLocaleString()">will be replaced by current time</span></div>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>

## Explanation

In the [Title Update Backend](/examples/title_update_backend) example we showed how to update the title of the page using a server sent event fragment. In this example we show how to update the title of the page using a requestAnimationFrame event on the client side.

```html
<div
  data-signals="{currentTime:Date.now()}"
  data-on-raf="currentTime = Date.now()"
>
  <div id="time">
    Current Time:
    <span data-text="new Date(currentTime).toLocaleString()"
      >will be replaced by current time</span
    >
  </div>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>
```

`data-on-raf` is a special event that is triggered on every requestAnimationFrame event. This is useful for updating the UI at maximum at the rendering refresh rate of the browser. In this example we update the currentTime signals with a new Date object. This triggers a re-render of the currentTime span element. You can still use the `__throttle` and `__debounce` modifiers to control the rate of updates even further.

In this case we are updating the currentTime signals with the current time. This triggers a re-render of the `currentTime` span element, however if you inspect with the browser debugger you will notice that `#time`'s `<span>` element is not updated every frame. This is because the signals are smartly updated only when the value changes. This is a performance optimization that is done by default.
