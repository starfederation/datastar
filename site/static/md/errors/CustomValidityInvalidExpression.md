# Error: CustomValidityInvalidExpression

An invalid expression was provided to the `data-custom-validity` attribute. The expression must evaluate to a string that will be set as the custom validity message. If the string is empty, the input is considered valid. If the string is non-empty, the input is considered invalid and the string is used as the reported message.

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