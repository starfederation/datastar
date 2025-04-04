# Attribute Plugins

Datastar provides the following
[`data-*`](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes) attributes.

### Core Attributes

- [`data-signals`](#data-signals)
- [`data-computed`](#data-computed)
- [`data-star-ignore`](#data-star-ignore)

### DOM Attributes

- [`data-attr`](#data-attr)
- [`data-bind`](#data-bind)
- [`data-class`](#data-class)
- [`data-on`](#data-on)
- [`data-ref`](#data-ref)
- [`data-show`](#data-show)
- [`data-text`](#data-text)

### Backend Attributes

- [`data-indicator`](#data-indicator)

### Browser Attributes

- [`data-custom-validity`](#data-custom-validity)
- [`data-on-intersect`](#data-on-intersect)
- [`data-on-interval`](#data-on-interval)
- [`data-on-load`](#data-on-load)
- [`data-on-raf`](#data-on-raf)
- [`data-on-signal-change`](#data-on-signal-change)
- [`data-persist`](#data-persist)
- [`data-replace-url`](#data-replace-url)
- [`data-scroll-into-view`](#data-scroll-into-view)
- [`data-view-transition`](#data-view-transition)

<div class="alert alert-info my-8">
  <iconify-icon icon="simple-icons:rocket"></iconify-icon>
  <div>
    The Datastar <a href="https://marketplace.visualstudio.com/items?itemName=starfederation.datastar-vscode">VSCode
      extension</a> and <a href="https://plugins.jetbrains.com/plugin/26072-datastar-support">IntelliJ plugin</a>
    provide autocompletion for all <code>data-*</code> attributes.
  </div>
</div>

### Attribute Order

<em>`data-*` attributes are evaluated in the order they appear in the DOM.</em>

Elements are evaluated by walking the DOM in a depth-first manner, and attributes are processed in the order they appear
in the element. This means that if you use a signal in a [Datastar expression](/guide/datastar_expressions), it must be
defined _before_ it is used.

```html
<!-- This works: -->
<div data-signals-foo="1" data-text="$foo"></div>

<!-- This works: -->
<div data-signals-foo="1"></div>
<div data-text="$foo"></div>

<!-- This works: -->
<div data-signals-foo="1">
  <div data-text="$foo"></div>
</div>

<!-- This does NOT work: -->
<div data-text="$foo" data-signals-foo="1"></div>

<!-- This does NOT work: -->
<div data-text="$foo"></div>
<div data-signals-foo="1"></div>
```

### Attribute Casing

<em>`data-*` attributes have special casing rules.</em>

[According to the HTML specification](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*)</em>,
all `data-*` atttributes (not Datastar the framework, but any time a data attribute appears in the DOM) are case
in-sensitive, but are converted to [camelCase](https://developer.mozilla.org/en-US/docs/Glossary/Camel_case) when
accessed from JavaScript by Datastar.

Datastar handles casing of data attributes in two ways:

1. **Signal names**: the keys used in attribute plugins that define signals (`data-signals-*`, `data-computed-*`,
`data-ref-*`, etc), are, by default, converted to camelCase. For example, `data-signals-my-signal` defines a signal
named `mySignal`. You would use the signal in a [Datastar expression](/guide/datastar_expressions) as `$mySignal`.

2. **All other attribute plugins**: the keys used by all other attribute plugins are, by default, converted to
[kebab-case](https://developer.mozilla.org/en-US/docs/Glossary/Kebab_case). For example, `data-class-text-blue-700` adds
or removes the class `text-blue-700`, and `data-on-rocket-launched` would react to the event named `rocket-launched`.

You can use the [`__case` modifier](#modifiers) to convert between camelCase, kebab-case, snake_case, and PascalCase, or
alternatively use object syntax when available.

For example, if a web component exposes an event `widgetLoaded`, you would use `data-on-widget-loaded__case.camel` to
react to it. Whereas, if you wanted to use a signal named `my-signal` then you would use the kebab modfier:
`data-signals-my-signal__case.kebab`.

## Core Attributes

The core attribute plugins are included in every bundle, and contain the core functionality in Datastar.

### `data-signals`

Merges one or more signals into the existing signals. Values defined later in the DOM tree override those defined
earlier.

```html
<div data-signals-foo="baz"></div>
```

Signals can be namespaced using dot-notation.

```html
<div data-signals-foo.bar="baz"></div>
```

Note when working with namespaced signals that only the leaf nodes are actually signals. So in the example above, only
`bar` is a signal, meaning that while using `$foo.bar` in an expression is possible, using `$foo` (the namespace) is
not.

The `data-signals` attribute can also be used with an object notation. This format can be to merge multiple signals
using a set of key-value pairs, where the keys represent signal names and the values represent expressions. This format
is also required to set signals to types other than a string.

```html
<div data-signals="{foo: {bar: 1, baz: false}}"></div>
```

The value above is written in JavaScript object notation, but JSON, which is a subset and which most templating
languages have built-in support for, is also allowed.

Keys used in `data-signals-*` are converted to camel case, so the signal name `mySignal` must be written as
`data-signals-my-signal` or `data-signals="{mySignal: 1}"`.

Signals beginning with an underscore are considered _local signals_ and are not included in requests to the backend by
default. You can include them by setting the [`includeLocal`](/reference/action_plugins#options) option to `true`.

Signal names cannot begin or contain double underscores (`__`), due to its use as a modifer delimiter.

#### Modifiers

Modifiers allow you to modify behavior when merging signals.

- `__case` - Converts the casing of the signal name.
- `.camel` - Camel case: `mySignal` (default)
- `.kebab` - Kebab case: `my-signal`
- `.snake` - Snake case: `my_signal`
- `.pascal` - Pascal case: `MySignal`
- `__ifmissing` - Only merges signals if their keys do not already exist. This is useful for setting defaults without
overwriting existing values.

```html
<div data-signals-my-signal__case.kebab="1" data-signals-foo__ifmissing="1"></div>
```

When supplying signals in bulk with object notation, modifiers can also be used:

```html
<!-- Merges the signal `mySignal` -->
<div data-signals="{mySignal: 'value'}"></div>

<!-- Merges the signal `mySignal` only if it doesn't already exist -->
<div data-signals__ifmissing="{mySignal: 'init-value'}"></div>

<!-- Defines a kebab cased signal `my-signal` using object notation -->
<div data-signals="{'my-signal': 'value'}"></div>

<!-- It is possible to set both `data-signals__ifmissing` and `data-signals` on the same element -->
<div data-signals="{'my-signal': 'value'}" data-signals__ifmissing="{widgetStatus: 'initial'}">
</div>
```

### `data-computed`

Creates a signal that is computed based on an expression. The computed signal is read-only, and its value is
automatically updated when any signals in the expression are updated.

```html
<div data-computed-foo="$bar + $baz"></div>
```

Computed signals are useful for memoizing expressions containing other signals. Their values can be used in other
expressions.

```html
<div data-computed-foo="$bar + $baz"></div>
<div data-text="$foo"></div>
```

`data-computed` is a pure reactive function, this has several implications:

1. If a computed signal is not consumed, then the computation will not execute.
2. Computed signals must not be used for performing actions (changing other signals, actions, JavaScript functions,
etc.).

```html
<!-- This computation will never execute because $foo is not used anywhere -->
<div data-computed-foo="$bar + $baz"></div> <!-- WRONG -->

<!-- Computed signals must *not* be used for side effects -->
<div data-computed-qux="@post('/qux'); 'quxed'"></div> <!-- WRONG -->
<div data-computed-foo="$bar++"></div> <!-- WRONG -->
```

If you find yourself wanting to perform some action in reaction to a signal change, refer to the
[`data-on-signal-change`](#data-on-signal-change) attribute.

#### Modifiers

Modifiers allow you to modify behavior when defining computed signals.

- `__case` - Converts the casing of the signal name.
- `.camel` - Camel case: `mySignal` (default)
- `.kebab` - Kebab case: `my-signal`
- `.snake` - Snake case: `my_signal`
- `.pascal` - Pascal case: `MySignal`

```html
<div data-computed-my-signal__case.kebab="$bar + $baz"></div>
```

### `data-star-ignore`

Datastar walks the entire DOM and applies plugins to each element it encounters. It's possible to tell Datastar to
ignore an element and its descendants by placing a `data-star-ignore` attribute on it. This can be useful for preventing
naming conflicts with third-party libraries, or when you are unable to [escape user
input](/reference/security#escape-user-input).

```html
<div data-star-ignore data-show-thirdpartylib>
  <div data-show-thirdpartylib>
    These element will not be processed by Datastar.
  </div>
</div>
```

#### Modifiers

- `__self` - Only ignore the element itself, not its descendants.

## DOM Attributes

Allow the usage of signals and expressions to affect the DOM.

### `data-attr`

Sets the value of any HTML attribute to an expression, and keeps it in sync.

```html
<div data-attr-title="$foo"></div>
```

The `data-attr` attribute can also be used to set the values of multiple attributes on an element using a set of
key-value pairs, where the keys represent attribute names and the values represent expressions.

```html
<div data-attr="{title: $foo, disabled: $bar}"></div>
```

### `data-bind`

Creates a signal (if one doesn't already exist) and sets up two-way data binding between it and an element's value. This
means that the value of the element is updated when the signal changes, and the signal is updated when the value of the
element changes.

The `data-bind` attribute be placed on any HTML element on which data can be input or choices selected from (`input`,
`select`,`textarea` elements, and web components). Event listeners are added for `change`, `input` and `keydown` events.

```html
<input data-bind-foo />
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the
templating language you are using.

```html
<input data-bind="foo" />
```

The initial value of the signal is set to the value of the element, unless a signal has already been defined. So in the
example below, `$foo` is set to `bar`.

```html
<input data-bind-foo value="bar" />
```

Whereas in the example below, `$foo` inherits the value `baz` of the predefined signal.

```html
<div data-signals-foo="baz">
  <input data-bind-foo value="bar" />
</div>
```

Multiple input values can be assigned to a single signal by predefining the signal as an array. So in the example below,
`$foo` is set to `['bar', 'baz']` when both checkboxes are checked.

```html
<div data-signals-foo="[]">
  <input data-bind-foo type="checkbox" value="bar" />
  <input data-bind-foo type="checkbox" value="baz" />
</div>
```

#### Modifiers

Modifiers allow you to modify behavior when binding signals.

- `__case` - Converts the casing of the signal name.
- `.camel` - Camel case: `mySignal` (default)
- `.kebab` - Kebab case: `my-signal`
- `.snake` - Snake case: `my_signal`
- `.pascal` - Pascal case: `MySignal`

```html
<input data-bind-my-signal__case.kebab />
```

### `data-class`

Adds or removes a class to or from an element based on an expression.

```html
<div data-class-hidden="$foo"></div>
```

If the expression evaluates to `true`, the `hidden` class is added to the element; otherwise, it is removed.

The `data-class` attribute can also be used to add or remove multiple classes from an element using a set of key-value
pairs, where the keys represent class names and the values represent expressions.

```html
<div data-class="{hidden: $foo, 'font-bold': $bar}"></div>
```

#### Modifiers

Modifiers allow you to modify behavior defining a class name.

- `__case` - Converts the casing of the class.
- `.camel` - Camel case: `myClass`
- `.kebab` - Kebab case: `my-class` (default)
- `.snake` - Snake case: `my_class`
- `.pascal` - Pascal case: `MyClass`

```html
<div data-class-my-class__case.camel="$foo"></div>
```

### `data-on`

Attaches an event listener to an element, executing an expression whenever the event is triggered.

```html
<button data-on-click="$foo = ''">Reset</button>
```

An `evt` variable that represents the event object is available in the expression.

```html
<div data-on-myevent="$foo = evt.detail"></div>
```

The `data-on` attribute works with [built-in events](https://developer.mozilla.org/en-US/docs/Web/Events) and [custom
events](https://developer.mozilla.org/en-US/docs/Web/Events/Creating_and_triggering_events). Note that the
`data-on-submit` event listener prevents the default submission behavior of forms.

#### Modifiers

Modifiers allow you to modify behavior when events are triggered. Some modifiers have tags to further modify the
behavior.

- `__once` \* - Only trigger the event listener once.
- `__passive` \* - Do not call `preventDefault` on the event listener.
- `__capture` \* - Use a capture event listener.
- `__case` - Converts the casing of the event.
- `.camel` - Camel case: `myEvent`
- `.kebab` - Kebab case: `my-event` (default)
- `.snake` - Snake case: `my_event`
- `.pascal` - Pascal case: `MyEvent`
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
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is
available.
- `__window` - Attaches the event listener to the `window` element.
- `__outside` - Triggers when the event is outside the element.
- `__prevent` - Calls `preventDefault` on the event listener.
- `__stop` - Calls `stopPropagation` on the event listener.

\* Only works on built-in events.

```html
<div data-on-click__window__debounce.500ms.leading="$foo = ''" data-on-my-event__case.camel="$foo = ''"></div>
```

### `data-ref`

Creates a new signal that is a reference to the element on which the data attribute is placed.

```html
<div data-ref-foo></div>
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the
templating language you are using.

```html
<div data-ref="foo"></div>
```

The signal value can then be used to reference the element.

```html
`$foo` holds a <span data-text="$foo.tagName"></span> element.
```

#### Modifiers

Modifiers allow you to modify behavior when defining references.

- `__case` - Converts the casing of the signal name.
- `.camel` - Camel case: `mySignal` (default)
- `.kebab` - Kebab case: `my-signal`
- `.snake` - Snake case: `my_signal`
- `.pascal` - Pascal case: `MySignal`

```html
<div data-ref-my-signal__case.kebab></div>
```

### `data-show`

Show or hides an element based on whether an expression evaluates to `true` or `false`. For anything with custom
requirements, use [`data-class`](#data-class) instead.

```html
<div data-show="$foo"></div>
```

To prevent flickering of the element before Datastar has processed the DOM, you can add a `display: none` style to the
element to hide it initially.

```html
<div data-show="$foo" style="display: none"></div>
```

### `data-text`

Binds the text content of an element to an expression.

```html
<div data-text="$foo"></div>
```

## Backend Attributes

Add integrations with [backend plugin actions](/reference/action_plugins#backend-plugins).

### `data-indicator`

Creates a signal and sets its value to `true` while an SSE request request is in flight, otherwise `false`. The signal
can be used to show a loading indicator.

```html
<button data-on-click="@get('/endpoint')" data-indicator-fetching></button>
```

This can be useful for show a loading spinner, disabling a button, etc.

```html
<button data-on-click="@get('/endpoint')" data-indicator-fetching data-attr-disabled="$fetching"></button>
<div data-show="$fetching">Loading...</div>
```

The signal name can be specified in the key (as above), or in the value (as below). This can be useful depending on the
templating language you are using.

```html
<button data-indicator="fetching"></button>
```

#### Modifiers

Modifiers allow you to modify behavior when defining indicator signals.

- `__case` - Converts the casing of the signal name.
- `.camel` - Camel case: `mySignal` (default)
- `.kebab` - Kebab case: `my-signal`
- `.snake` - Snake case: `my_signal`
- `.pascal` - Pascal case: `MySignal`

## Browser Attributes

### `data-custom-validity`

Allows you to add custom validity to an element using an expression. The expression must evaluate to a string that will
be set as the custom validity message. If the string is empty, the input is considered valid. If the string is
non-empty, the input is considered invalid and the string is used as the reported message.

```html
<form>
  <input data-bind-foo name="foo" />
  <input data-bind-bar name="bar" data-custom-validity="$foo === $bar ? '' : 'Field values must be the same.'" />
  <button>Submit form</button>
</form>
```

### `data-on-intersect`

Runs an expression when the element intersects with the viewport.

```html
<div data-on-intersect="$intersected = true"></div>
```

#### Modifiers

Modifiers allow you to modify the element intersection behavior and the timing of the event listener.

- `__once` - Only triggers the event once.
- `__half` - Triggers when half of the element is visible.
- `__full` - Triggers when the full element is visible.
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
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is available.

```html
<div data-on-intersect__once__full="$fullyIntersected = true"></div>
```

### `data-on-interval`

Runs an expression at a regular interval. The interval duration defaults to 1 second and can be modified using the `__duration` modifier.

```html
<div data-on-interval="$count++"></div>
```

#### Modifiers

Modifiers allow you to modify the interval duration.

- `__duration` - Sets the interval duration.
  - `.500ms` - Interval duration of 500 milliseconds.
  - `.1s` - Interval duration of 1 second (default).
  - `.leading` - Execute the first interval immediately.
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is available.

```html
<div data-on-interval__duration.500ms="$count++"></div>
```

### `data-on-load`

Runs an expression when the element is loaded into the DOM.

```html
<div data-on-load="$count = 1"></div>
```

#### Modifiers

Modifiers allow you to add a delay to the event listener.

- `__delay` - Delay the event listener.
  - `.500ms` - Delay for 500 milliseconds.
  - `.1s` - Delay for 1 second.
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is available.

```html
<div data-on-load__delay.500ms="$count = 1"></div>
```

### `data-on-raf`

Runs an expression on every [`requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame) event.

```html
<div data-on-raf="$count++"></div>
```

#### Modifiers

Modifiers allow you to modify the timing of the event listener.

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
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is available.

```html
<div data-on-raf__debounce.10ms="$count++"></div>
```

### `data-on-signal-change`

Runs an expression whenever a signal changes.

```html
<div data-on-signal-change="$count++"></div>
```

A key can be provided to only trigger the event when a specific signal changes.

```html
<div data-on-signal-change-foo="$fooCount++"></div>
```

You can use `*` to match a single path segment and `**` to match multiple path segments.

```html
<!-- Listen for changes to `$foo.bar.baz` -->
<div data-signals-foo.bar.baz="1"
     data-on-signal-change-foo.*.baz="$fooCount++"
     data-on-signal-change-foo.**="$fooCount++"
></div>
```

#### Modifiers

Modifiers allow you to modify the timing of the event listener.

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
- `__viewtransition` - Wraps the expression in `document.startViewTransition()` when the View Transition API is available.

```html
<div data-on-signal-change__debounce.100ms="$count++"></div>
```

### `data-persist`

Persists signals in [local storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage). This is useful for storing values between page loads.

```html
<div data-persist></div>
```

If one or more space-separated values are provided as a string, only those signals are persisted.

```html
<div data-persist="foo bar"></div>
```

You can use `*` to match a single path segment and `**` to match multiple path segments.

```html
<!-- Persists `$foo.bar.baz` -->
<div data-signals-foo.bar.baz="1"
     data-persist="foo.*.baz"
></div>

<!-- Persists `$foo.bar.baz` -->
<div data-signals-foo.bar.baz="1"
     data-persist="foo.**"
></div>
```

#### Modifiers

Modifiers allow you to modify the storage target.

- `__session` - Persists signals in [session storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage).

```html
<div data-persist__session="foo bar"></div>
```

### `data-replace-url`

Replaces the URL in the browser without reloading the page. The value can be a relative or absolute URL, and is an
evaluated expression.

```html
<div data-replace-url="`/page${page}`"></div>
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

### `data-view-transition`

Sets the `view-transition-name` style attribute explicitly.

```html
<div data-view-transition="$foo"></div>
```

Page level transitions are automatically handled by an injected meta tag. Inter-page elements are automatically
transitioned if the [View Transition API](https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API) is
available in the browser and `useViewTransitions` is `true`.

## Aliasing Data Attributes

It is possible to alias `data-*` attributes to a custom alias (`data-foo-*`, for example) using the [bundler](/bundler).
A custom alias should _only_ be used if you have a conflict with a legacy library and
[`data-star-ignore`](#data-star-ignore) cannot be used.

We maintain a `data-star-*` aliased version that can be included as follows.

```html
<script type="module"
  src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.11/bundles/datastar-aliased.js"></script>
```