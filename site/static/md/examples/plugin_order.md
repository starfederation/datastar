## Plugin Order

## Demo

<div>
  <label>Works</label>
  <div data-signals-foo="1" data-text="$foo"></div>
  <div class="divider"></div>
  <label>Doesn't Work</label>
  <div data-text="$bar" data-signals-bar="2"></div>
</div>

## Explanation

```html
<label>Works</label>
<div data-signals-foo="1" data-text="$foo"></div>
<div class="divider"></div>
<label>Doesn't Work</label>
<div data-text="$bar" data-signals-bar="2"></div>
``

The order of plugins is important but luckily in your control.  Plugins are applied first in depth first HTML or SVG element order, then by the order of attributes prefixed with `data-*`.

In this example, the `data-signals-foo` attribute is applied first, then the `data-text` attribute is applied.  This is why the `data-text` attribute is able to reference the `foo` signal.