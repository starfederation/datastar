## Signals Change Path

## Demo

<div data-signals="{foo: {bar: 0, baz: 0}}" data-on-signals-change-foo="alert('foo changed')">
  <div class="flex flex-col md:flex-row gap-4">
    <button data-on-click="$foo.bar++" class="btn btn-primary">
      Increment $foo.bar
    </button>
    <button data-on-click="$foo.baz++" class="btn btn-primary">
      Increment $foo.baz
    </button>
  </div>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>

## Explanation

```html
<div
  data-signals="{foo: {bar: 0, baz: 0}}"
  data-on-signals-change-foo="alert('foo changed')"
>
    <button data-on-click="$foo.bar++" class="btn btn-primary">Increment $foo.bar</button>
    <button data-on-click="$foo.baz++" class="btn btn-primary">Increment $foo.baz</button>
</div>
<pre data-text="ctx.signals.JSON()"></pre>
```