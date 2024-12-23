# Error: AttributesValueRequired

No value was provided to the `data-attr` attribute. The `data-attr` attribute _must_ have a value, representing either an expression (if a key is provided), or an object of key-value pairs, where the keys are attribute names and the values are expressions.

Example:

```html
<div data-attr-disabled="foo.value"></div>

<div data-attr="{disabled: foo.value}"></div>
```

See the docs on the [`data-attr`](/reference/attribute_plugins#data-attr) attribute.