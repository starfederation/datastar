# Error: SignalsValueRequired

No value was provided to the `data-signals` attribute. The `data-signals` attribute _must_ have a value, representing either an expression (if a key is provided), or an object of key-value pairs, where the keys are signal names and the values are expressions.

Example:

```html
<div data-signals-foo="''"></div>

<div data-signals-foo="1"></div>

<div data-signals="{foo: 1}"></div>
```

See the docs on the [`data-signals`](/reference/attribute_plugins#data-signals) attribute.