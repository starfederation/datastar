## Polling

## Demo

<div data-signals-count="0" data-on-interval__duration.1s.leading="@get('/examples/polling/interval')"
  class="text-primary">
  Count: <span data-text="$count"></span>
</div>

## Explanation

```html
<div data-signals-count="0" data-on-interval__duration.1s.leading="@get('/endpoint')">
  Count: <span data-text="$count"></span>
</div>
```

You can use the [`data-on-interval`](/reference/attribute_plugins#data-on) attribute to execute an expression at a
regular interval. In this example, we are sending a `get` request to the backend that increments the `$count` signal
every 1 second. Adding `.leading` to the `__duration` modifier executes the first interval immediately.

An alternative approach is use the [`data-init`](/reference/attribute_plugins#data-on) attribute with the `__delay`
modifier to control the polling frequency from the backend. Using this method, you can control back-pressure.

```html
<div data-signals-count="0" data-init__delay.1s="@get('/endpoint')">
  Count: 0
</div>
```

The backend endpoint can merge the same fragment but with a different delay value to determine the polling frequency.