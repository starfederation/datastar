# Error: SseClosestFormNotFound

No closest form could be found in the DOM. When setting the `contentType` option to `form` in the `@{ action }()` action, the element must have a wrapping form, otherwise a `selector` must be provided to a form that exists in the DOM.

Example using a wrapping form:

```html
<form>
    <button data-on-click="@{ action }('/endpoint', {contentType: 'form'})"></div>
</form>
```

Example using a selector:

```html
<button data-on-click="@{ action }('/endpoint', {contentType: 'form', selector: '#myform'})"></div>
```

See the options available on the [`@get()`](/reference/action_plugins#get) action.