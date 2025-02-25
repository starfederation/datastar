# Expression Context

Datastar is _not_ exposed in the global scope. Everything you need to do should be possible via Datastar expressions in [attribute plugins](/reference/attribute_plugins) and [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE events.

Every Datastar expression is evaluated using a context `ctx`. This means that `ctx` is exposed in expressions and can be used to access properties and methods within the current context.

You should use `ctx` sparingly, and only when you can't achieve the desired behavior using signals.

### `ctx.el`

The current element being processed.

### `ctx.plugin`

The current plugin being processed.

### `ctx.signals`

The signals root object that contains functions for accessing and modifying signals. Signals can be accessed using the `signal` method. The following is the equivalent of an expression containing only `$foo`.

```html
<div data-signals-foo="1">
  <div data-text="ctx.signals.signal('foo').value"></div>
</div>
```

Here is how you can output the existing signals in JSON format, which can be useful when troubleshooting an issue.

```html
<pre data-text="ctx.signals.JSON()"></pre>
```
