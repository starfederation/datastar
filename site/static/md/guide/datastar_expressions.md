# Datastar Expressions

Datastar expressions are strings that are evaluated by Datastar attributes and actions. 

The following example outputs `1` not because `$foo` is defined in the global scope (it's not), but because we've defined `foo` as a signal with the initial value `1`, and are using `$foo` in a `data-*` attribute.

```html
<div data-signals-foo="1">
  <div data-text="$foo"></div>
</div>
```

When Datastar evaluates the expression `$foo`, it first converts it to `ctx.signals.signal('foo').value`, and then evaluates that expression in a sandboxed context, in which `ctx` represents the Datastar context. 

This means that JavaScript can be used in Datastar expressions.

```html
<div data-text="$foo.length"></div>
```

In the above expression, `$foo.length` is first converted to `ctx.signals.signal('foo').value.length` and then evaluated as follows.

```js
return (()=> {
  return (ctx.signals.signal('foo').value.length);
})()
```

This should help clarify what Datastar is doing behind the scenes, how signals are evaluated in expressions, and also why expressions are limited in scope. 

The following example is invalid because the namespace `$foo` is _not_ a signal – only the leaf nodes are signals.

```html
<div data-signals-foo.bar="1">
  <div data-text="$foo"></div>
</div>
```

The following example is valid because both `$foo.bar` and `$baz` are signals.

```html
<div data-signals-foo.bar="1" data-signals-baz="1">
  <div data-text="$foo.bar"></div>
  <div data-text="$baz"></div>
</div>
```

Note that `data-*` attributes are evaluated in the order they appear in the DOM, so the `data-text` attributes must come _after_ the `data-signals-*` attributes in the example above. See the [attribute plugins reference](/reference/attribute_plugins) for more information.

Multiple statements can be used in a single expression by separating them with a semicolon.

```html
<div data-signals-foo="1">
  <button data-on-click="$foo++; @post('/endpoint')"></button>
</div>
```

Expressions may span multiple lines, but a semicolon must be used to separate statements.

```html
<div data-signals-foo="1">
  <button data-on-click="
        $foo++; 
        @post('/endpoint')
  "></button>
</div>
```

See the [Datastar Context reference](/reference/datastar_context) for more advanced usage of Datastar expressions.