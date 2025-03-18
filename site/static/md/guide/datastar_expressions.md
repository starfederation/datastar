# Datastar Expressions

Datastar expressions are strings that are evaluated by Datastar attributes and actions. While they are very similar to JavaScript, there are some important differences that make them more powerful for declarative hypermedia applications.

- [Datastar Expressions](#datastar-expressions)
  - [Basic Usage](#basic-usage)
  - [Namespaced Signals](#namespaced-signals)
  - [Multiple Statements and Formatting](#multiple-statements-and-formatting)
  - [Event Context](#event-context)
  - [Summary](#expression-summary)


## Basic Usage

The following example outputs `1` not because `$foo` is defined in the global scope (it's not), but because we've defined `foo` as a signal with the initial value `1`, and are using `$foo` in a `data-*` attribute.

```html
<div data-signals-foo="1">
  <div data-text="$foo"></div>
</div>
```

When Datastar evaluates the expression `$foo`, it first converts it to `ctx.signals.signal('foo').value`, and then evaluates that expression in a sandboxed context, in which `ctx` represents the current [expression context](/reference/expression_context). 

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

Unlike JavaScript variables, signal names, when kebab cased, can dashes that would be invalid in standard JavaScript identifiers:

```html
<div data-signals-user-name="John">
  <button data-on-click="$user-name = 'Alice'">Become Alice</button>
  <!-- This works even though "user-name" would be invalid as a JavaScript variable -->
</div>
```

This works because behind the scenes, Datastar is transforming the datastar expression into the javascript `ctx.signals.signal('user-name').value = "Alice"` - the signal name is passed as a string, not used as a direct JavaScript identifier.

This should help clarify what Datastar is doing behind the scenes, how signals are evaluated in expressions, and also why expressions are limited in scope.

## Namespaced Signals

The following example is invalid because the namespace `$foo` is _not_ a signal – only the leaf nodes are signals.

```html
<div data-signals-foo.bar="1">
  <div data-text="$foo"></div> <!-- Invalid -->
</div>
```

The following example is valid because both `$foo.bar` and `$baz` are signals.

```html
<div data-signals-foo.bar="1" data-signals-baz="1">
  <div data-text="$foo.bar"></div> <!-- Valid -->
  <div data-text="$baz"></div> <!-- Valid -->
</div>
```

Note that `data-*` attributes are evaluated in the order they appear in the DOM, so the `data-text` attributes must come _after_ the `data-signals-*` attributes in the example above. See the [attribute plugins reference](/reference/attribute_plugins) for more information.

## Multiple Statements and Formatting

Multiple statements can be used in a single expression by separating them with a semicolon.

```html
<div data-signals-foo="1">
  <button data-on-click="$foo++; @post('/endpoint')"></button>
</div>
```

Expressions may span multiple lines, but a semicolon must be used to separate statements. Unlike JavaScript, line breaks alone are not sufficient to separate statements.

```html
<div data-signals-foo="1">
  <button data-on-click="
        $foo++; 
        @post('/endpoint')
  "></button>
</div>
```

There are two things to be aware about the final (or only!) statement in a datastar expression:

1. The final statement in an expression does not require a semicolon
2. The return value of the final statement is implicitly returned.

The return value of an expression is often discarded, but depending on which attribute plugin you are using, it may be relevant.

```html
<button data-on-click="
      $count++;
      $message = 'Clicked ' + $count + ' times'
"></button>
```

## Event Context

When using `data-on-*` attributes, the event object is available as `evt`:

```html
<input data-on-input="$value = evt.target.value">
```

This gives you access to the standard browser event properties and methods inside your expressions.

## Expression Summary

For quick reference, here are the key aspects of Datastar expressions:

- **Signal Access:** Signals are accessed using the `$` prefix (e.g., `$count`, `$user-name`, `$items.selected`)
  - JS global identifiers starting with `$` are not usable in Datastar expressions
- **Case Sensitivity:** Signal names are case-sensitive, just like in the attribute they were defined in
- **Actions:** [Actions](/reference/overview#action-plugins) are called using the `@` prefix (e.g., `@post('/api/save')`)
- **Statement Delimiter:** Statements must be separated by semicolons (`;`), not just line breaks
- **Implicit Return:** The last statement is automatically returned without needing the `return` keyword
- **Special Variables:**
  - `evt`: The event object (available in `data-on-*` attributes)
  - `ctx`: The [expression context](/reference/expression_context) with access to signals and utilities
