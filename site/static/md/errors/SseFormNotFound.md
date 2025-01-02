# Error: SseFormNotFound

No form with the selector `{ selector }` could be found in the DOM. When specifying a form selector using the `selector` option in the `@{ action }()` action, the form must already exist in the DOM.

Example:

```html
<button data-on-click="@{ action }('/endpoint', {contentType: 'form', selector: '#myform'})"></div>
```

See the options available on the [`@get()`](/reference/action_plugins#get) action.