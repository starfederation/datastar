## Lazy Load

[Original HTMX Version](https://htmx.org/examples/lazy-load/)

## Demo

<div id="lazy_load" data-init="@get('/examples/lazy_load/data')">
</div>

## Explanation

This example shows how to lazily load an element on a page. We start with an initial state that looks like this:

```html
<div data-init="@get('/examples/lazy_load/graph')">
  Loading...
</div>
```

Which shows a progress indicator as we are loading the graph. The graph is then loaded.