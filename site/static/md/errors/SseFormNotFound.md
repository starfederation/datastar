# Error: SseFormNotFound

No form with the selector `{ selector }` could be found in the DOM. When specifying a form selector using the `selector` option in the `@sse()` action, the form must already exist in the DOM.

Example:

```html
<button data-on-click="@sse('/endpoint', {contentType: 'form', selector: '#myform'})"></div>
```

See the docs on the [`@sse()`](/reference/action_plugins#sse) action.