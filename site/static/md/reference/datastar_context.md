# Datastar Context

Datastar is _not_ exposed in the global scope. Everything you need to do should be possible via Datastar expressions in [attribute plugins](/reference/attribute_plugins) and [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE events.

Every Datastar expression is evaluated in the context of `ctx`, which is the Datastar context. This means that `ctx` can be used in expressions to access context properties and methods.

You should use `ctx` sparingly, and only when you can't achieve the desired behavior using signals.

### `ctx.el`

Returns the current element being processed.

### `ctx.plugin`

Returns the current plugin being processed.

### `ctx.signals`

`ctx.signals` is the signals root object that contains functions for accessing and modifying signals. Signals can be accessed using the `signal` method, which is the equivalent of what `$foo` does.

```html
<div data-signals-foo="1">
  <div data-text="ctx.signals.signal('foo').value"></div>
</div>
```

And here is how you can output the existing signals in JSON format, which can be useful when troubleshooting an issue.

```html
<pre data-text="ctx.signals.JSON()"></pre>
```

## Security

When using a [Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP) (CSP), `unsafe-eval` must be allowed for scripts, since Datastar evaluates expressions inline. 

```html
<meta http-equiv="Content-Security-Policy" 
      content="script-src 'self' 'unsafe-eval';"
>
```
