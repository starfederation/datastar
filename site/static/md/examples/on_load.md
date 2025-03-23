## On load

## Demo

<div id="replaceMe" data-init="@post('/examples/on_load/data')">
  No session data
</div>

## Explanation

```html
<div id="replaceMe" data-init="@post('/examples/on_load/data')">
  No session data
</div>
```

The `data-init` attribute is used to specify a fetch request that should be made when the element is loaded. The value
of the attribute is a JavaScript expression that is evaluated when the element is loaded.

The following example adds a delay of 3 seconds.

```html
<div id="replaceMe" data-init__delay.3s="@post('/examples/on_load/data')">
  No session data
</div>
```