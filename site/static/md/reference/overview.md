# Overview

- [Attribute Plugins](#attribute-plugins)
- [Action Plugins](#action-plugins)
- [Expression Context](#expression-context)
- [Server-Sent Events](#server-sent-events)
- [SDKs](#sdks)
- [Security](#security)
- [Custom Builds](#custom-builds)

## Attribute Plugins

Attribute plugins are `data-*` attributes that add reactive behavior to HTML elements.

### Core Attributes

| Attribute | Description |
|-----------|-------------|
| [`data-signals`](/reference/attribute_plugins#data-signals) | Merges one or more signals into the existing signals. |
| [`data-computed`](/reference/attribute_plugins#data-computed) | Creates a read-only signal computed from an expression. |
| [`data-star-ignore`](/reference/attribute_plugins#data-star-ignore) | Ignores an element and its descendants when processing. |

### DOM Attributes

| Attribute | Description |
|-----------|-------------|
| [`data-attr`](/reference/attribute_plugins#data-attr) | Sets any HTML attribute value using expressions. |
| [`data-bind`](/reference/attribute_plugins#data-bind) | Creates two-way data binding between a signal and an element's value. |
| [`data-class`](/reference/attribute_plugins#data-class) | Adds or removes CSS classes based on expressions. |
| [`data-on`](/reference/attribute_plugins#data-on) | Attaches event listeners that execute expressions. |
| [`data-persist`](/reference/attribute_plugins#data-persist) | Persists signals in Local Storage or Session Storage. |
| [`data-ref`](/reference/attribute_plugins#data-ref) | Creates a signal reference to the DOM element. |
| [`data-replace-url`](/reference/attribute_plugins#data-replace-url) | Replaces the URL in the browser without page reload. |
| [`data-text`](/reference/attribute_plugins#data-text) | Binds text content of an element to an expression. |

### Browser Attributes

| Attribute | Description |
|-----------|-------------|
| [`data-custom-validity`](/reference/attribute_plugins#data-custom-validity) | Adds custom validation to elements. |
| [`data-intersects`](/reference/attribute_plugins#data-intersects) | Runs expressions when elements intersect the viewport. |
| [`data-scroll-into-view`](/reference/attribute_plugins#data-scroll-into-view) | Scrolls elements into view with various options. |
| [`data-show`](/reference/attribute_plugins#data-show) | Shows or hides elements based on expressions. |
| [`data-view-transition`](/reference/attribute_plugins#data-view-transition) | Sets `view-transition-name` for View Transitions API. |

### Backend Attributes

| Attribute | Description |
|-----------|-------------|
| [`data-indicator`](/reference/attribute_plugins#data-indicator) | Creates signals that indicate when SSE requests are in flight. |

View the [attribute plugins reference](/reference/attribute_plugins)

## Action Plugins

Action plugins are used in Datastar expressions to perform specific actions.

| Action | Description |
|--------|-------------|
| [`@get()`](/reference/action_plugins#get) | Sends a GET request to the backend and merges the response. |
| [`@post()`](/reference/action_plugins#post) | Sends a POST request to the backend and merges the response. |
| [`@put()`](/reference/action_plugins#put) | Sends a PUT request to the backend and merges the response. |
| [`@patch()`](/reference/action_plugins#patch) | Sends a PATCH request to the backend and merges the response. |
| [`@delete()`](/reference/action_plugins#delete) | Sends a DELETE request to the backend and merges the response. |

### Browser Actions

| Action | Description |
|--------|-------------|
| [`@clipboard()`](/reference/action_plugins#clipboard) | Copies the provided evaluated expression to the clipboard. |

### Utility Actions

| Action | Description |
|--------|-------------|
| [`@fit()`](/reference/action_plugins#fit) | Makes a value linearly interpolate from an original range to a new one. |
| [`@setAll()`](/reference/action_plugins#setall) | Sets all signals with a specific prefix to a provided value. |
| [`@toggleAll()`](/reference/action_plugins#toggleall) | Toggles all signals that start with a given prefix. |

View the [action plugins reference](/reference/action_plugins)

## Expression Context

Datastar expressions have access to a context object (`ctx`) providing:
- Current element reference (`ctx.el`)
- Signals root object for accessing and modifying signals (`ctx.signals`)

View the [expression context reference](/reference/expression_context)

## Server-Sent Events

Datastar uses Server-Sent Events (SSE) to communicate from the server to the client.

| Event Type | Description |
|------------|-------------|
| [`datastar-merge-fragments`](/reference/sse_events#datastar-merge-fragments) | Merges HTML fragments into the DOM. |
| [`datastar-merge-signals`](/reference/sse_events#datastar-merge-signals) | Updates signals with new values. |
| [`datastar-remove-fragments`](/reference/sse_events#datastar-remove-fragments) | Removes HTML fragments matching selectors. |
| [`datastar-remove-signals`](/reference/sse_events#datastar-remove-signals) | Removes signals matching specific paths. |
| [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) | Executes JavaScript in the browser. |

View the [SSE events reference](/reference/sse_events)

## SDKs

Officially supported SDKs for generating Datastar-specific SSE events:
- [Clojure](/reference/sdks#clojure)
- [.NET](/reference/sdks#net)
- [Go](/reference/sdks#go)
- [Haskell](/reference/sdks#haskell)
- [Java](/reference/sdks#java)
- [PHP](/reference/sdks#php) (with Laravel and Craft CMS packages)
- [Python](/reference/sdks#python)
- [Rust](/reference/sdks#rust)
- [Ruby](/reference/sdks#ruby)
- [TypeScript](/reference/sdks#typescript)
- [Zig](/reference/sdks#zig)

View the [SDK reference](/reference/sdks)

## Security

Security guidelines for using Datastar expressions safely:

| Consideration | Description |
|---------------|-------------|
| Escape User Input | Always escape user input to prevent XSS attacks when using Datastar expressions. |
| Avoid Sensitive Data | Signal values are visible in source code and can be modified, avoid leaking sensitive data. |
| Ignore Unsafe Input | Use `data-star-ignore` to ignore unsafe content that cannot be escaped. |
| Content Security Policy | Requires 'unsafe-eval' for script sources since Datastar evaluates expressions using IIFE. |

View the [security reference](/reference/security)

## Custom Builds

Datastar is built using a modular architecture that allows you to create custom builds with only the plugins you need, useful for reducing the framework's footprint.

View the [custom builds reference](/reference/custom_builds)
