# Going Deeper

Datastar's philosophy is: let the browser do what it does best—render HTML—while enabling declarative reactivity.

At its core, Datastar makes __namespaced signals declarative__. Let's unpack that.

### 1. Declarative

Declarative code is amazing. It allows you to simply request the result you want, without having to think about the steps required to make it happen.

Consider this imperative (non-declarative) way of conditionally placing a class on an element using JavaScript.

```js
if (foo == 1) {
  document.getElementById('myelement').classList.add('bold');
}
else {
  document.getElementById('myelement').classList.remove('bold');
}
```

Datastar allows us to write this logic declaratively while embracing locality-of-behavior, by placing it directly on the element we want to affect.

```html
<div data-class-bold="$foo == 1"></div>
```

### 2. Signals

Datastar uses signals, provided by [Preact Signals](https://preactjs.com/guide/v10/signals/), to manage state. You can think of signals as reactive variables that automatically track and propagate changes in expressions.

Signals can be created and modified using `data-*` attributes on the frontend, or events sent from the backend. They can also be used in [Datastar expressions](/guide/datastar_expressions).

```html
<div data-signals-foo="hello"></div>
<div data-text="$foo"></div>
<button data-on-click="$foo = ''"></button>
```

Behind the scenes, Datastar converts `$foo` to `ctx.signals.signal('foo').value`, and then evaluates the expression in a sandboxed context. This means that JavaScript can be used in Datastar expressions.

```html
<button data-on-click="$foo = $foo.toUpperCase()">
  Convert to uppercase
</button>
```

### 3. Namespaced Signals

Signals in Datastar have a trick up their sleeve: they can be namespaced. 

```html
<div data-signals-foo.bar="1"></div>
```

Or, using object syntax:

```html
<div data-signals="{foo: {bar: 1}}"></div>
```

Or, using two-way binding:

```html
<input data-bind-foo.bar />
```

Note that only the leaf nodes are actually signals. So in the example above, `bar` is a signal but `foo`(the namespace) is not, meaning that while using `$foo.bar` in an expression is possible, using `$foo` is not.

Namespaced signals can be useful for targetting signals in a more granular way on the backend.

Another practical use-case might be when you have repetition of state on a page. 

The following example shows how to toggle the value of all signals starting with `menu.open.` at once when a button is clicked.

```html
<div data-signals="{menu: {isopen: {desktop: false, mobile: false}}}">
  <button data-on-click="@toggleAll('menu.isopen.')">
    Open/close menu
  </button>
</div>
```

The beauty of this is that you don't need to write a bunch of code to set up and maintain state. You just use `data-*` attributes and think declaratively!

## Datastar Actions

Actions are helper functions that can be used in [Datastar expressions](/guide/datastar_expressions). They allow you to perform logical operations without having to write procedural JavaScript.

```html
<button data-on-click="@setAll('foo.', $mysignal.toUpperCase())">
  Convert all to uppercase
</button>
```

### Backend Actions

The [`@get()`](/reference/action_plugins#get) action sends a `GET` request to the backend using `fetch`, and expects an event stream response containing zero or more [Datastar SSE events](/reference/sse_events).

```html
<button data-on-click="@get('/endpoint')"></button>
```

An event stream response is nothing more than a response containing a `Content-Type: text/event-stream` header.

SSE events can update the DOM, adjust signals, or run JavaScript directly in the browser.

```
event: datastar-merge-fragments
data: fragments <div id="hello">Hello, world!</div>

event: datastar-merge-signals
data: signals {foo: {bar: 1}}

event: datastar-execute-script
data: script console.log('Success!')
```

Using one of the backend [SDKs](/reference/sdks) will help you get up and running faster.

Here is all of the backend code required to produce the events above in each of the SDKs.

!!!CODE_SNIPPET:going_deeper/multiple_events!!!

Every request is sent with a `{datastar: *}` object that includes all existing signals (except for local signals whose keys begin with an underscore). This allows frontend state to be shared with the backend, and for the backend to “drive the frontend” (control its state and behavior dynamically).

## Embracing Simplicity

Datastar is smaller than Alpine.js and htmx, yet it provides the functionality of both libraries combined.

The package size is not _just_ a vanity metric. By embracing simplicity, and building on first principles, everything becomes cleaner and leaner. But don't take our word for it – [explore the source code](https://github.com/starfederation/datastar/tree/main/library) and see for yourself!

Datastar is both a core library (~5 KiB) and a “batteries included” framework (~13 KiB), allowing you to create [custom bundles](/bundler) and write your own plugins.

## Hypermedia First

Datastar is a hypermedia framework. Hypermedia is the idea that the web is a network of interconnected resources, and it is the reason the web has been so successful.

However, the rise of the frontend frameworks and SPAs has led to a lot of confusion about how to use hypermedia.

Browsers don't care about your application – they care about rendering hypermedia. For example, if you visit a membership website as a guest, you'll likely see a generic landing page and a login option. Only once you log in will you see links to member-only content. This has huge benefits.

- Each interaction determines the next valid state.
- When implemented correctly, all logic resides in the backend, eliminating the need for frontend routing, validation, etc.
- HTML can be generated from any language.

## Unlearning

When approaching Datastar, especially when coming from other frontend frameworks, be prepared to _unlearn_ some bad practices. These may not seem like bad practices initially; they may even feel natural to you. Here are a few things you should look out for.

1. __Overuse of procedural code for DOM manipulation.__ Avoid writing procedural JavaScript to manually update the DOM. Use declarative, HTML-based `data-*` attributes and SSE events instead.
2. __Putting state and logic in signals.__ Avoid recreating the sins of SPAs by putting state and logic in signals. Signals should only exist for what users can interact with, and for sharing state with the backend.
3. __Managing state on the frontend.__ Avoid excessive frontend state management. Instead, let the backend drive state by managing data persistence and logic, ensuring a single source of truth. Focus on keeping the frontend lightweight and reactive.

We're very confident that Datastar can do _anything_ that React, Vue.js, or Svelte can do, faster and with less code. We'll take on anyone that disagrees!

When you embrace hypermedia, everything becomes much _less_ complicated. Put state in the right place, and it becomes a lot easier to reason about.
