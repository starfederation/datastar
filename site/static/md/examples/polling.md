## Polling

## Demo

<div 
  data-signals-count="0"
  data-on-interval__duration.2s="@get('/examples/polling/interval')"
  class="text-primary"
>
  Count: <span data-text="$count"></span>
</div>

## Explanation

```html
<div 
    data-signals-count="0"
    data-on-interval__duration.2s="@get('/endpoint')"
>
    Count: <span data-text="$count"></span>
</div>
```

You can use the [`data-on-interval`](/reference/attribute_plugins#data-on) attribute to execute an expression at a regular interval. In this example, we are sending a `get` request to the backend every 2 seconds, which increments the `$count` signal.

An alternative approach is use the [`data-on-load`](/reference/attribute_plugins#data-on) attribute with the `__delay` modifier to control the polling frequency from the backend. Using this method, you can control back-pressure.

```html
<div 
    data-signals-count="0"
    data-on-load__delay.2s="@get('/endpoint')"
>
    Count: 0
</div>
```

The backend endpoint can merge the same fragment but with a different delay value to determine the polling frequency.