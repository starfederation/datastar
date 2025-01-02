# Debounce & Throttling

Debouncing and throttling are two techniques to limit the number of times a function is called. They are often used in scenarios where a function is called frequently, such as in response to user input.

## Throttling

<div class="bg-secondary text-secondary-content p-8 rounded-box font-bold font-mono text-6xl" data-signals="{tick:0}" data-on-raf__throttle.500ms="$tick = (new Date()).getTime()">
<div data-text="$tick">Text</div>
</div>

```html
<div
    data-signals="{tick:0}"
    data-on-raf__throttle.500ms="$tick = (new Date()).getTime()"
>
    <div data-text="$tick"></div>
</div>
```

In the example above, the `data-on-raf__throttle.500ms` directive ensures that the `tick` value is updated at most once every 500ms. If the value is updated more frequently, the updates are ignored until the 500ms delay has passed.

### Debouncing

<button class="btn btn-primary btn-lg" data-signals="{clicks:0}" data-on-click__debounce.500ms="$clicks++">Debounced button clicked<span data-text="$clicks"> times</span>
</button>

```html
<button
    data-signals="{clicks:0}"
    data-on-click__debounce.500ms="$clicks++"
>
    Debounced button clicked
    <span data-text="$clicks"></span>
    times
</button>
```

In the example above, the `data-on-click__debounce.500ms` directive ensures that the click handler is only called once every 500ms. If the button is clicked multiple times within 500ms, the click handler will only be called once after the 500ms delay with no additional clicks.
