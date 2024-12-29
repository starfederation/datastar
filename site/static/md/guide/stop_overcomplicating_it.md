# Stop Overcomplicating It

Most of the time, if you run into issues when using Datastar, **you are probably overcomplicating it™**. 

As explained in [going deeper](/guide/going_deeper), Datastar is a hypermedia framework. As soon as you approach it like a JavaScript framework, you are likely going to run into complications.

So how does one use a hypermedia framework?

## The Datastar Way

Between [attribute plugins](/reference/attribute_plugins) and [action plugins](/reference/action_plugins), Datastar provides you with everything you need to build practically anything. Any additional JavaScript you require should ideally be extracted out into encapsulated scripts or, better yet, web components. 

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        Always encapsulate state and send <strong><em>props down, events up</em></strong>.
    </div>
</div>

### External Scripts

When using external scripts, pass data into functions via arguments and listen for custom events dispatched from them (_props down, events up_).

In this way, the function is encapsulated – all it knows is that it receives input via an argument, acts on it, and optionally dispatches a custom event containing the result – and Datastar's `data-*` attributes can be used to drive reactivity.

```html
<div data-signals-result="''"
     data-on-mycustomevent__window="result.value = evt.detail.value"
>
  <input data-bind-foo 
         data-on-input="myfunction(foo.value)"
  >
  <span data-text="result.value"></span>
</div>
```

```js
function myfunction(data) {
  const value = `You entered ${data}`;
  window.dispatchEvent(
    new CustomEvent('mycustomevent', {detail: {value}})
  );
}
```

### Web Components

[Web components](https://developer.mozilla.org/en-US/docs/Web/API/Web_components) allow you create reusable, encapsulated, custom elements. They are native to the web and require no external libraries or frameworks. Web components unlock [custom elements](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements) – HTML tags with custom behavior and styling.

When using web components, pass data into them via attributes and listen for custom events dispatched from them (_props down, events up_).

In this way, the web component is encapsulated – all it knows is that it receives input via an attribute, acts on it, and optionally dispatches a custom event containing the result – and Datastar's `data-*` attributes can be used to drive reactivity.

```html
<div data-signals-result="''">
  <input data-bind-foo>
  <my-component
      data-attributes-src="foo.value"
      data-on-mycustomevent="result.value = evt.detail.value"
  ></my-component>
  <span data-text="result.value"></span>
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

Since the `value` attribute is allowed on web components, it is also possible to use `data-bind` to bind a signal to the web component's value. Note that a `change` event must be dispatched so that the event listener used by `data-bind` registers the value change.

```html
<input data-bind-foo>
<my-component
    data-attributes-src="foo.value"
    data-bind-result
></my-component>
<span data-text="result.value"></span>
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

Datastar is a tiny self-contained framework that liberates you from the constraints of dependencies. If, for some peculiar reason, you absolutely _must_ use a third-party library, you should continue using the _props down, events up_ pattern.

For edge-cases where you find yourself having to change the DOM without involving Datastar, you can import Datastar and apply it to any part of the DOM. for more details, see the [JavaScript API](/reference/javascript_api).