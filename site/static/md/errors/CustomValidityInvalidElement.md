# Error: CustomValidityInvalidElement

The `data-custom-validity` attribute was used on an element that is not a `HTMLInputElement`. The `data-custom-validity` attribute can only be used on form elements.

Example:

```html
<form>
  <input data-bind-foo data-custom-validity="foo === bar ? '' : 'Field values must be the same.'" name="foo">
  <input data-bind-bar name="bar">
  <button>
      Submit form
  </button>
</form>
```

See the docs on the [`data-custom-validity`](/reference/attribute_plugins#data-custom-validity) attribute.