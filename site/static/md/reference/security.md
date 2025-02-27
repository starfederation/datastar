# Security

[Datastar expressions](/guide/datastar_expressions) are strings that are evaluated in a sandboxed context, in which `ctx` represents the Datastar context. This means that JavaScript can be used in Datastar expressions. 

## Escape User Input

The golden rule of security is to never trust user input. This is especially true when using Datastar expressions, which can execute arbitrary JavaScript. 

When using Datastar expressions, you should always escape user input. This is to prevent, among other issues, Cross Site Scripting (XSS) attacks.

Keep in mind that signal values are visible in the source code in plain text, and can be modified by the user before being sent in requests. For that reason, you should avoid using sensitive data in signals and always implement backend validation.

## Ignore Unsafe Input

If, for some reason, you cannot escape unsafe user input, you should ignore it using the [`data-star-ignore`](/reference/attribute_plugins#data-star-ignore) attribute. This tells Datastar to ignore an element and its descendants when processing DOM nodes.

## Content Security Policy

When using a [Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP) (CSP), `unsafe-eval` must be allowed for scripts, since Datastar evaluates expressions inline. 

```html
<meta http-equiv="Content-Security-Policy" 
      content="script-src 'self' 'unsafe-eval';"
>
```
