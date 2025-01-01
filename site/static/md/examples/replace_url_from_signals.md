## Replace URL from Signals

## Demo

<div
    data-signals="{page: 0}"
    data-on-raf__throttle.1s="$page++"
    data-replace-url="`/examples/replace_url_from_signals?page=${$page}`"
>
</div>

# Look at the URL in the browser address bar!

## Explanation

```html
<div
    data-signals="{page: 0}"
    data-on-raf__throttle.1s="$page++"
    data-replace-url="`/examples/replace_url_from_signals?page=${$page}`"
>
</div>
```

The `data-replace-url` attribute is a special attribute that is used to replace the URL in the browser without reloading the page. This is useful for updating the URL when the user interacts with the page. In this example we update the URL with the current page number every second. This is done by incrementing the `page` signals every second. The `data-on-raf__throttle.1s` event is triggered every second and increments the `page` signals. This triggers a re-render of the `page` signals and updates the URL in the browser. You can still use the `__throttle` and `__debounce` modifiers to control the rate of updates even further.
