# Expression Context

Datastar is _not_ exposed in the global scope. Everything you need to do should be possible via Datastar expressions in [attribute plugins](/reference/attribute_plugins) and [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE events.

Every Datastar expression is evaluated using a context `ctx`. This means that `ctx` is exposed in expressions and can be used to access properties and methods within the current context.

You should use `ctx` sparingly, and only when you can't achieve the desired behavior using signals.

### `ctx.el`

The current element being processed.

```html
<button data-on-click="ctx.el.disabled = true"></button>
```