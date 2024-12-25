# Attribute Plugins

Datastar provides the following [`data-*`](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes) attributes.

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        The Datastar <a href="https://marketplace.visualstudio.com/items?itemName=starfederation.datastar-vscode">VSCode extension</a> and <a href="https://plugins.jetbrains.com/plugin/26072-datastar-support">IntelliJ plugin</a> provided autocompletion for all <code>data-*</code> attributes.
    </div>
</div>

## Core Plugins

[Source Code](https://github.com/starfederation/datastar/blob/main/library/src/plugins/official/core/attributes)

The core plugins are included in every bundle, and contain the core functionality in Datastar.

### `data-signals`

Merges one or more signals into the existing signals. Values defined later in the DOM tree override those defined earlier.

```html
<div data-signals-foo="1"></div>
```

Signals are nestable using dot-notation, which can be useful for namespacing.

```html
<div data-signals-foo.bar="1"></div>
```

The `data-signals` attribute can also be used to merge multiple signals using a set of key-value pairs, where the keys represent signal names and the values represent expressions.

```html
<div data-signals="{foo: {bar: 1, baz: 2}}"></div>
```

The value above is written in Javascript object notation, but JSON, which is a subset and which most templating languages have built-in support for, is also allowed.

Note that `data-*` attributes are case-insensitive. If you want to use uppercase characters in signal names, you’ll need to kebabize them or use the object syntax. So the signal name `mySignal` must be written as `data-signals-my-signal` or `data-signals="{mySignal: 1}"`.

#### Modifiers

Modifiers allow you to modify behavior when merging signals.

- `__ifmissing` - Only merges signals if their keys do not already exist. This is useful for setting defaults without overwriting existing values.

```html
<div data-signals-foo__ifmissing="1"></div>
```

### `data-computed`

Creates a signal that is computed based on an expression. The computed signal is read-only, and its value is automatically updated when any signals in the expression are updated.

```html
<div data-computed-foo="bar.value + baz.value"></div>
```

Computed signals are useful for memoizing expressions containing other signals. Their values can be used in other expressions.

```html
<div data-computed-foo="bar.value + baz.value"></div>
<div data-text="foo.value"></div>
```

### `data-ref`

Creates a new signal that is a reference to the element on which the data attribute is placed.

```html
<div data-ref-foo></div>
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the templating language you are using.

```html
<div data-ref="foo"></div>
```

The signal value can then be used to reference the element.

```html
`foo` holds a <span data-text="foo.value.tagName"></span> element.
```

## DOM Plugins

[Source Code](https://github.com/starfederation/datastar/blob/main/library/src/plugins/official/dom/attributes)

Allows the usage of signals and expressions to affect the DOM.

### `data-attr`

Binds the value of any HTML attribute to an expression.

```html
<div data-attr-title="foo.value"></div>
```

The `data-attr` attribute can also be used to set the values of multiple attributes on an element using a set of key-value pairs, where the keys represent attribute names and the values represent expressions.

```html
<div data-attr="{title: foo.value, disabled: bar.value}"></div>
```

### `data-bind`

Creates a signal and sets up two-way data binding between it and an element's value. Can be placed on any HTML element on which data be be input or choices selected from (`input`, `textarea`, `select`, `checkbox` and `radio` elements, as well as web components).

```html
<input data-bind-foo />
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the templating language you are using.

```html
<input data-bind="foo" />
```

**Note:** Event listeners are added for `change`, `input` and `keydown` events on `input`,`textarea`, `select`, `checkbox` and `radio` elements.

### `data-class`

Adds or removes a class to or from an element based on an expression.

```html
<div data-class-hidden="foo.value"></div>
```

If the expression evaluates to `true`, the `hidden` class is added to the element; otherwise, it is removed.

The `data-class` attribute can also be used to add or remove multiple classes from an element using a set of key-value pairs, where the keys represent class names and the values represent expressions.

```html
<div data-class="{hidden: foo.value, bold: bar.value}"></div>
```

### `data-on`

Attaches an event listener to an element, executing an expression whenever the event is triggered.

```html
<button data-on-click="foo.value = ''">Reset</button>
```

An `evt` variable that represents the event object is available in the expression.

```html
<div data-on-myevent="foo.value = evt.detail.value"></div>
```

The `data-on` attribute matches DOM events, however there are currently a few special cases for custom events.

1. `data-on-load` is triggered when an element is loaded into the DOM.
2. `data-on-signals-change` is triggered when any signals change.
3. `data-on-raf` is triggered on every `requestAnimationFrame` event.

Note that the `data-on-submit` event listener now prevents the default submission behavior of forms.

#### Modifiers

Modifiers allow you to modify behavior when events are triggered. Some modifiers have tags to further modify the behavior.

- `__once` \* - Only trigger the event listener once.
- `__passive` \* - Do not call `preventDefault` on the event listener.
- `__capture` \* - Use a capture event listener.
- `__debounce` - Debounce the event listener.
  - `.500ms` - Debounce for 500 milliseconds.
  - `.1s` - Debounce for 1 second.
  - `.leading` - Debounce with leading edge.
  - `.notrail` - Debounce without trailing edge.
- `__throttle` - Throttle the event listener.
  - `.500ms` - Throttle for 500 milliseconds.
  - `.1s` - Throttle for 1 second.
  - `.noleading` - Throttle without leading edge.
  - `.trail` - Throttle with trailing edge.
- `__window` - Attaches the event listener to the `window` element.
- `__outsite` - Triggers when the event is outside the element.
- `__prevent` - Calls `preventDefault` on the event listener.
- `__stop` - Calls `stopPropagation` on the event listener.

\* Only works on native events.

```html
<div data-on-click__window__debounce.500ms.leading="foo.value = ''"></div>
```

### `data-persist`

Persists signals in Local Storage. This is useful for storing values between page loads.

```html
<div data-persist></div>
```

If one or more space-separated values are provided as a string, only those signals are persisted.

```html
<div data-persist="'foo bar'"></div>
```

If a key is provided, it will be used as the key when saving in storage, otherwise `datastar` will be used.

```html
<div data-persist-mykey="'foo bar'"></div>
```

#### Modifiers

Modifiers allow you to modify the storage target.

- `__session` - Persists signals in Session Storage.

```html
<div data-persist__session></div>
```

### `data-replace-url`

Replaces the URL in the browser without reloading the page. The value can be a relative or absolute URL, and is an evaluated expression.

```html
<div
  data-replace-url="`/page${page.value}`">
</div>
```

### `data-text`

Binds the text content of an element to an expression.

```html
<div data-text="foo.value"></div>
```

## Browser Plugins

[Source Code](https://github.com/starfederation/datastar/tree/main/library/src/plugins/official/browser/attributes)

Focused on showing and hiding elements based on signals. Most of the time you want to send updates from the server but is useful for things like modals, dropdowns, and other UI elements.

### `data-custom-validity`

Allows you to add custom validity to an element using an expression. The expression must evaluate to a string that will be set as the custom validity message. If the string is empty, the input is considered valid. If the string is non-empty, the input is considered invalid and the string is used as the reported message.

```html
<form>
  <input data-bind-foo data-custom-validity="foo.value === bar.value ? '' : 'Field values must be the same.'" name="foo">
  <input data-bind-bar name="bar">
  <button>
      Submit form
  </button>
</form>
```

### `data-intersects`

Runs an expression when the element intersects with the viewport.

```html
<div data-intersects="intersected.value = true"></div>
```

#### Modifiers

Modifiers allow you to modify the element intersection behavior.

- `__once` - Only triggers the event once.
- `__half` - Triggers when half of the element is visible.
- `__full` - Triggers when the full element is visible.

```html
<div data-intersects__once="intersected.value = true"></div>
```

### `data-scroll-into-view`

Scrolls the element into view. Useful when updating the DOM from the backend, and you want to scroll to the new content.

```html
<div data-scroll-into-view></div>
```

#### Modifiers

Modifiers allow you to modify scrolling behavior.

- `__smooth` - Scrolling is animate smoothly.
- `__instant` - Scrolling is instant.
- `__auto` - Scrolling is determined by the computed `scroll-behavior` CSS property.
- `__hstart` - Scrolls to the left of the element.
- `__hcenter` - Scrolls to the horizontal center of the element.
- `__hend` - Scrolls to the right of the element.
- `__hnearest` - Scrolls to the nearest horizontal edge of the element.
- `__vstart` - Scrolls to the top of the element.
- `__vcenter` - Scrolls to the vertical center of the element.
- `__vend` - Scrolls to the bottom of the element.
- `__vnearest` - Scrolls to the nearest vertical edge of the element.
- `__focus` - Focuses the element after scrolling.

```html
<div data-scroll-into-view__smooth></div>
```

### `data-show`

Show or hides an element based on whether an expression evaluates to `true` or `false`. For anything with custom requirements, use [`data-class`](#data-class) instead.

```html
<div data-show="foo.value"></div>
```

### `data-view-transition`

Sets the `view-transition-name` style attribute explicitly.

```html
<div data-view-transition="foo"></div>
```

Page level transitions are automatically handled by an injected meta tag. Inter-page elements are automatically transitioned if the [View Transition API](https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API) is available in the browser and `useViewTransitions` is `true`.

## Backend Plugins

[Source Code](https://github.com/starfederation/datastar/blob/main/library/src/plugins/official/backend/attributes)

Add integrations with the [`sse()`](/reference/action_plugins#sse) action.

### `data-indicator`

Creates a signal and sets its value to `true` while an SSE request request is in flight, otherwise `false`. The signal can be used to show a loading indicator.

```html
<button
  data-on-click="sse('/endpoint')"
  data-indicator-fetching
></button>
```

This can be useful for show a loading spinner, disabling a button, etc.

```html
<button
  data-on-click="sse('/endpoint')"
  data-indicator-fetching
  data-attr-disabled="fetching.value"
></button>
<div data-show="fetching.value">Loading...</div>
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the templating language you are using.

```html
<button data-indicator="fetching"></button>
```

## Ignoring Elements

### `data-star-ignore`

Datastar walks the entire DOM and applies plugins to each element it encounters. It's possible to tell Datastar to ignore an element by placing a `data-star-ignore` attribute on it. This can be useful for preventing naming conflicts with third-party libraries.

```html
<div data-star-ignore data-show-thirdpartylib>
  This element’s attributes will not be processed by Datastar.
</div>
```