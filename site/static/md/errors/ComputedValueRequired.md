# Error: ComputedValueRequired

No value was provided to the `data-computed` attribute. The `data-computed` attribute _must_ have a value, representing the expression to assign to the signal.

Example:

```html
<div data-computed-blinker="$count % 2 === 0"></div>
```

See the docs on the [`data-computed`](/reference/attribute_plugins#data-computed) attribute.