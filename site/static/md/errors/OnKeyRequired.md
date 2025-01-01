# Error: OnKeyRequired

No key was provided to the `data-on` attribute. The `data-on` attribute _must_ have a key, representing the event listener to attach to the element.

Example:

```html
<button data-on-click="alert($foo)">Click Me</button>
```

See the docs on the [`data-on`](/reference/attribute_plugins#data-on) attribute.