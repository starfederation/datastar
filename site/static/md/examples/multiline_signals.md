## Multi-line Signals

## Demo

<div
    data-signals="{
        foo: 1,
        bar: 'bar'
    }"
    data-computed-baz="$foo * 2"
>
    <input
        id="a"
        type="number"
        step="1"
        min="0"
        data-bind-foo
        class="input input-bordered"
    />
    <br>
    <input
        type="text"
        data-bind-baz
        class="input input-bordered"
    />
    <br>
    <input
        id="b"
        type="text"
        data-bind-bar
        class="input input-bordered"
    />
</div>

## Explanation

```html
<div
  data-signals="{
    foo: 1234,
    bar: 'bar'
}"
>
  <div data-computed-baz="$foo * 2"></div>
  <input type="number" step="1" min="0" data-bind-foo />
  <input type="text" data-bind-baz />
  <input type="text" data-bind-bar />
</div>
```