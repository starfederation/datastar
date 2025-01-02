## On load

## Demo

<div>
<div
  id="replaceMe"
  data-on-load="@post('/examples/on_load/data')"
  >No session data</div>
</div>

## Explanation

```html
<div id="replaceMe" data-on-load="@post('/examples/on_load/data')">
  No session data
</div>
```

The `data-on-load` attribute is used to specify a fetch request that should be made when the element is loaded. The value of the attribute is a JavaScript expression that is evaluated when the element is loaded. This example comes from a [GitHub issue](https://github.com/starfederation/datastar/issues/15) where the user wanted to load session data when the page was loaded.

**Note:** In this case its targeting the `#replaceMe` element but its up to the SSE returned to how the page is updated.
