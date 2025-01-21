## Polling

## Demo

<div
    data-signals="{count: 0}"
    data-on-interval__delay.1s="$count++" 
    class="text-primary"
>
    Count: <span data-text="$count"></span>
</div>

## Explanation

```html
<div
    data-signals="{count: 0}"
    data-on-interval__delay.1s="$count++"
>
    Count: <span data-text="$count">Count</span>
</div>
```

You can use the [`data-on-interval`](/reference/attribute_plugins#data-on) attribute to execute an expression at a regular interval. In this example, we are incrementing the `count` signal every 1 second.

An alternative approach is use the [`data-on-load`](/reference/attribute_plugins#data-on) attribute with the `__delay` modifier to control the polling frequency from the backend. Using this method, you can control back-pressure.

```html
<div data-on-load__delay.1s="@get('/endpoint')">
    Count: 0
</div>
```

The backend endpoint can merge the same fragment but with a different delay value to determine the polling frequency.