# Error: SseInvalidContentType

An invalid option value `contentType: '{ contentType }'` was passed into the `sse()` action. Acceptable content types are `json` (default) and `form`.

Example:

```html
<button data-on-click="sse('/endpoint', {contentType: 'form'})"></div>
```

See the docs on the [`sse()`](/reference/action_plugins#sse) action.