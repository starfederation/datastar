## Custom Validity

## Demo

<form data-signals-bar="" data-on-submit="@get('/examples/custom_validity/data', {contentType: 'form'})" class="space-y-8">
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-foo data-custom-validity="$foo === $bar ? '' : 'Field values must be the same.'" name="foo" class="grow" placeholder="Type foo contents" required />
  </label>
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-bar name="bar" class="grow" placeholder="Type bar contents" />
  </label>
  <div class="space-x-4">
    <button class="btn btn-primary">
      Submit form
    </button>
  </div>
</form>

## Explanation

The expression passed into the `data-custom-validity` attribute is evaluated and the result is used as the custom validity message for the input. If the result is an empty string, the input is considered valid. If the result is a non-empty string, the input is considered invalid and the string is used as the custom validity message.

Note that the signal `bar` must be defined _before_ it is referenced in the `data-custom-validity` attribute expression.

```html
<form data-signals-bar="" data-on-submit="@get('/endpoint', {contentType: 'form'})">
  <input data-bind-foo data-custom-validity="$foo === $bar ? '' : 'Field values must be the same.'" name="foo" required />
  <input data-bind-bar name="bar" />
  <button>
    Submit form
  </button>
</form>
```