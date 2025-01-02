# Error: SseInvalidContentType

An invalid option value `contentType: '{ contentType }'` was passed into the `@{ action }()` action. Acceptable content types are `json` (default) and `form`.

Example:

```html
<button data-on-click="@{ action }('/endpoint', {contentType: 'form'})"></div>
```

See the options available on the [`@get()`](/reference/action_plugins#get) action.