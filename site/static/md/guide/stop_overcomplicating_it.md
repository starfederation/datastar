# Stop Overcomplicating It

Most of the time, if you run into issues when using Datastar, **you are probably overcomplicating it™**. 

As explained in [going deeper](/guide/going_deeper), Datastar is a _hypermedia_ framework. If you approach it like a _JavaScript_ framework, you are likely to run into complications.

So how does one use a hypermedia framework?

## The Datastar Way

Between [attribute plugins](/reference/attribute_plugins) and [action plugins](/reference/action_plugins), Datastar provides you with everything you need to build hypermedia-driven applications. Using this approach, the backend drives state to the frontend and acts as the single source of truth, determining what actions the user can take next.

Any additional JavaScript functionality you require that does _not_ work via [`data-*`](/reference/attribute_plugins) attributes and [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE events should ideally be extracted out into [external scripts](#external-scripts) or, better yet, [web components](#web-components). 

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        Always encapsulate state and send <strong><em>props down, events up</em></strong>.
    </div>
</div>

### External Scripts

When using external scripts, pass data into functions via arguments and return a result _or_ listen for custom events dispatched from them (_props down, events up_).

In this way, the function is encapsulated – all it knows is that it receives input via an argument, acts on it, and optionally returns a result or dispatches a custom event – and `data-*` attributes can be used to drive reactivity.

```html
<div data-signals-result="''">
  <input data-bind-foo 
         data-on-input="$result = myfunction($foo)"
  >
  <span data-text="$result"></span>
</div>
```

```js
function myfunction(data) {
  return `You entered ${data}`;
}
```

If your function call is asynchronous then it will need to dispatch a custom event containing the result. While asynchronous code _can_ be placed within Datastar expressions, Datastar will _not_ await it.

```html
<div data-signals-result="''"
     data-on-mycustomevent__window="$result = evt.detail.value"
>
  <input data-bind-foo 
         data-on-input="myfunction($foo)"
  >
  <span data-text="$result"></span>
</div>
```

```js
async function myfunction(data) {
  const value = await new Promise((resolve) => {
    setTimeout(() => resolve(`You entered ${data}`), 1000);
  });
  window.dispatchEvent(
    new CustomEvent('mycustomevent', {detail: {value}})
  );
}
```

### Web Components

[Web components](https://developer.mozilla.org/en-US/docs/Web/API/Web_components) allow you create reusable, encapsulated, custom elements. They are native to the web and require no external libraries or frameworks. Web components unlock [custom elements](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements) – HTML tags with custom behavior and styling.

When using web components, pass data into them via attributes and listen for custom events dispatched from them (_props down, events up_).

In this way, the web component is encapsulated – all it knows is that it receives input via an attribute, acts on it, and optionally dispatches a custom event containing the result – and `data-*` attributes can be used to drive reactivity.

```html
<div data-signals-result="''">
  <input data-bind-foo>
  <my-component
      data-attr-src="$foo"
      data-on-mycustomevent="$result = evt.detail.value"
  ></my-component>
  <span data-text="$result"></span>
</div>
```

```js
class MyComponent extends HTMLElement {
  static get observedAttributes() {
    return ['src'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    const value = `You entered ${newValue}`;
    this.dispatchEvent(
      new CustomEvent('mycustomevent', {detail: {value}})
    );
  }
}

customElements.define('my-component', MyComponent);
```

Since the `value` attribute is allowed on web components, it is also possible to use `data-bind` to bind a signal to the web component's value. Note that a `change` event must be dispatched so that the event listener used by `data-bind` is triggered by the value change.

```html
<input data-bind-foo>
<my-component
    data-attr-src="$foo"
    data-bind-result
></my-component>
<span data-text="$result"></span>
```

```js
class MyComponent extends HTMLElement {
  static get observedAttributes() {
    return ['src'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    this.value = `You entered ${newValue}`;
    this.dispatchEvent(new Event('change'));
  }
}

customElements.define('my-component', MyComponent);
```

## Third-Party Libraries

Datastar is a tiny self-contained framework that can help liberate you from "dependency hell". If, for some reason, you absolutely _must_ use a third-party library, you should continue using the _props down, events up_ pattern whenever possible.

For edge-cases in which you find yourself having to change the DOM without involving Datastar, you can import Datastar and apply it to any part of the DOM. for more details, see the [JavaScript API](/reference/javascript_api).