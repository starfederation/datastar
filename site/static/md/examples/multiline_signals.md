## Multi-line Signals

## Demo

<div
    data-signals="{
        foo:1234,
        bar:'bar'
    }"
    data-signals-baz="2*foo"
>
    <input
        id="a"
        type="number"
        step="1"
        min="0"
        max="10"
        data-bind-foo
        class="input input-bordered"
    />
    <input
        id="b"
        type="text"
        data-bind-bar
        class="input input-bordered"
    />
    <input
        id="c"
        type="text"
        data-bind-baz
        class="input input-bordered"
    />
</div>

## Explanation

```html
<div
  data-signals="{
    foo:1234,
    bar:'bar'
}"
>
  <div data-signals-baz="2*foo"></div>
  <input type="number" step="1" min="0" max="10" data-bind-foo />
  <input type="text" data-bind-bar />
  <input type="text" data-bind-baz />
</div>
```

The `data-signals` attribute is used to define a signals object. The signals object is a JS object.

Also you'll see we have a `data-signals-baz` attribute. This is a signal that is dependent on the `foo` signal. The `foo` signal is defined in the signals object.  Since this isn't a computed signal, it will only use the initial value of `foo` to calculate the value of `baz`.
