# Datastar Expressions

Datastar expressions are strings that are evaluated by Datastar attributes and actions. While they are similar to JavaScript, there are some important differences that make them more powerful for declarative hypermedia applications.

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

When Datastar evaluates the expression `$foo`, it first converts it to `ctx.signals.signal('foo').value`, and then evaluates that expression in a sandboxed context, in which `ctx` represents the current expression context. 

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

Normal JavaScript operators are also available in Datastar expressions. This includes (but isn't limited to) `&&`, `||`, and the ternary operator. These last three can be useful when reacting to signal changes.

For example, the following would only trigger a post action if the signal is logically true.

```html
<button data-on-click="$landingGearRetracted && @post('/launch')"></div

<button data-on-click="$landingGearState == 'retracted' && @post('/launch')"></div
```

The ternary operator is useful to choose among two options:

```html
<div data-attr-class="$theme == 'dark' ? 'bg-black text-white' : 'bg-white text-black'"
```

## Namespaced Signals

The following example is invalid because the namespace `$foo` is _not_ a signal – only leaf nodes are signals.

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

- **Signals:** Signals are accessed using the `$` prefix (e.g., `$count`, `$userName`, `$items.selected`).
  - JS global identifiers starting with `$` are not usable in Datastar expressions.
- **Actions:** [Actions](/reference/action_plugins) are called using the `@` prefix (e.g., `@post('/endpoint')`).
- **Statement Delimiter:** Statements must be separated by semicolons (`;`), not just line breaks.
- **Implicit Return:** The last statement is automatically returned without needing the `return` keyword.
- **Special Variables:**
  - `evt`: The event object (available in `data-on-*` attributes).