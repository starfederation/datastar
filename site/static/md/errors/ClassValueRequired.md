# Error: ClassValueRequired

No value was provided to the `data-class` attribute. The `data-class` attribute _must_ have a value, representing either an expression (if a key is provided), or an object of key-value pairs, where the keys are class names and the values are expressions.

Example:

```html
<div data-class-hidden="foo"></div>

<div data-class="{hidden: foo}"></div>
```

See the docs on the [`data-class`](/reference/attribute_plugins#data-class) attribute.