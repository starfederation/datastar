## Invalid Signals

## Demo

<div data-signals="{foo: {bar: 1, baz: 2}}">
    <div data-text="$foo.woof"></div>
</div>

## Explanation

In the following example, `$foo` is not a valid signal, since only the leaf nodes in nested signals are actually signals. Open the console and you’ll see a link to a helpful error message.

```html
<div data-signals="{foo: {bar: 1, baz: 2}}">
    <div data-text="$foo.woof"></div>
</div>
```