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