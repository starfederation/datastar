## Aliased Data Attributes

## Demo

<div data-ds-signals-foo="1" data-ds-on-load__delay.1s="$foo++" class="text-primary">
  <button data-ds-on-click="$foo++" class="btn btn-primary">Increment</button>
  <pre data-ds-text="ctx.signals.JSON()"></pre>
</div>

## Explanation

This demo uses a custom bundle with the alias `ds`, meaning that data attributes must use the format `data-ds-*`.

```html
<div data-ds-signals-foo="1" data-ds-on-load__delay.1s="$foo++">
  <button data-ds-on-click="$foo++">Increment</button>
  <pre data-ds-text="ctx.signals.JSON()"></pre>
</div>
```