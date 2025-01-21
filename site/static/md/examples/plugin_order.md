## Plugin Order

## Demo

<div>
  <label>Works:</label>
  <span data-signals-foo="1" data-text="$foo"></span>
  <div class="divider"></div>
  <label>Does not work:</label>
  <span data-text="$bar" data-signals-bar="2"></span>
</div>

## Explanation

```html
Works:
<div data-signals-foo="1" data-text="$foo"></div>

Does not work (throws an error, see the console):
<div data-text="$bar" data-signals-bar="2"></div>
```

Note that `data-*` attributes are evaluated in the order they appear in the DOM. Elements are evaluated by walking the DOM in a depth-first manner, and attributes are processed in the order they appear in the element. This means that if you use a signal in a [Datastar expression](/guide/datastar_expressions), it must be defined _before_ it is used.

In this example, the `data-signals-foo` attribute is applied first, then the `data-text` attribute is applied.  This is why the `data-text` attribute is able to reference the `foo` signal.