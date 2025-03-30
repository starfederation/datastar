# Action Plugins

Datastar provides the following actions, that can be used in Datastar expressions.

### Backend Actions

- [`@get()`](#get)
- [`@post()`](#post)
- [`@put()`](#put)
- [`@patch()`](#patch)
- [`@delete()`](#delete)

### Browser Actions

- [`@clipboard()`](#clipboard)

### Utility Actions

- [`@setAll()`](#setall)
- [`@toggleAll()`](#toggleall)
- [`@fit()`](#fit)

## Backend Actions

Allow for the integration of any backend service that supports SSE.

### `@get()`

Arguments: `@get(url: string, options={})`

Sends a `GET` request to the backend using `fetch`, and merges the response with the current DOM and signals. The URL can be any valid URL and the response must contain zero or more [Datastar SSE events](/reference/sse_events).

```html
<button data-on-click="@get('/endpoint')"></button>
```

By default, all requests are sent with a `{datastar: *}` object containing the current signals (except for local signals whose keys begin with an underscore). When using a `get` request, the signals are sent as a query parameter, otherwise they are send as a JSON body.

It is possible to send form encoded requests by setting the `contentType` option to `form`. This sends GET requests using `application/x-www-form-urlencoded` encoding and non-GET requests using `multipart/form-data` encoding. See the [form data example](/examples/form_data).

Note that when a page is hidden (in a background tab, for example), the default behavior is for the SSE connection to be closed, and reopened when the page becomes visible again. To keep the connection open when the page is hidden, set the [`openWhenHidden`](#options) option to `true`.

### `@post()`

Arguments: `@post(url: string, options={})`

Works the same as `@get()` but sends a `POST` request to the backend.

```html
<button data-on-click="@post('/endpoint')"></button>
```

### `@put()`

Arguments: `@put(url: string, options={})`

Works the same as `@get()` but sends a `PUT` request to the backend.

```html
<button data-on-click="@put('/endpoint')"></button>
```

### `@patch()`

Arguments: `@patch(url: string, options={})`

Works the same as `@get()` but sends a `PATCH` request to the backend.

```html
<button data-on-click="@patch('/endpoint')"></button>
```

### `@delete()`

Arguments: `@delete(url: string, options={})`

Works the same as `@get()` but sends a `DELETE` request to the backend.

```html
<button data-on-click="@delete('/endpoint')"></button>
```

### Options

All of the actions above take a second argument of options.

- `contentType` - The type of content to send. A value of `json` sends all signals in a JSON request. A value of `form` tells the action to look for the closest form to the element on which it is placed (unless a `selector` option is provided), perform validation on the form elements, and send them to the backend using a form request (no signals are sent). Defaults to `json`.
- `includeLocal` - Whether to include local signals (those beggining with an underscore) in the request. Defaults to `false`.
- `selector` - Optionally specifies a form to send when the `contentType` option is set to `form`. If the value is `null`, the closest form is used. Defaults to `null`.
- `headers` - An object containing headers to send with the request.
- `openWhenHidden` - Whether to keep the connection open when the page is hidden. Useful for dashboards but can cause a drain on battery life and other resources when enabled. Defaults to `false`.
- `retryInterval` - The retry interval in milliseconds. Defaults to `1000` (1 second).
- `retryScaler` - A numeric multiplier applied to scale retry wait times. Defaults to `2`.
- `retryMaxWaitMs` - The maximum allowable wait time in milliseconds between retries. Defaults to `30000` (30 seconds).
- `retryMaxCount` - The maximum number of retry attempts. Defaults to `10`.
- `abort` - An [AbortSignal](https://developer.mozilla.org/en-US/docs/Web/API/AbortSignal) object that can be used to cancel the request.

```html
<div data-on-click="@get('/endpoint', {
  includeLocal: true,
  headers: {
    'X-Csrf-Token': 'JImikTbsoCYQ9oGOcvugov0Awc5LbqFsZW6ObRCxuqFHDdPbuFyc4ksPVVa9+EB4Ag+VU6rpc680edNFswIRwg==',
  },
  openWhenHidden: true,
})"></div>
```

### Events

All of the actions above trigger `datastar-sse` events during the SSE request lifecycle. The event type determines the stage of the request.

- `started` - Triggered when the SSE request is started.
- `finished` - Triggered when the SSE request is finished.
- `error` - Triggered when the SSE request encounters an error.
- `retrying` - Triggered when the SSE request is retrying.
- `retries-failed` - Triggered when the SSE request fails after retrying.

```html
<div data-on-datastar-sse="evt.detail.type == 'error' && console.log('SSE error encountered')"></div>
```

## Browser Actions

Actions for performing browser operations.

### `@clipboard()`

Arguments: `@clipboard(expression: string)`

Copies the provided evaluated expression to the clipboard.

```html
<div data-on-click="@clipboard('Hello, world!')"></div>
```

## Utility Actions

### `@setAll()`

Arguments: `@setAll(paths: string, value: any)`

Sets the value of all matching signals to the expression provided in the second argument. The first argument accepts one or more space-separated signal paths. You can use `*` to match a single segment and `**` to match multiple path segments.

```html
<!-- Sets the value of `$foo` -->
<div data-signals-foo="false">
  <button data-on-click="@setAll('foo', $bar)"></button>
</div>

<!-- Sets the value of `$foo.bar.baz` -->
<div data-signals-foo.bar.baz="false">
  <button data-on-click="@setAll('foo.*.baz', true)"></button>
  <button data-on-click="@setAll('foo.**', true)"></button>
</div>
```

### `@toggleAll()`

Arguments: `@toggleAll(paths: string)`

Toggles the value of all matching signals. The first argument accepts one or more space-separated signal paths. You can use `*` to match a single segment and `**` to match multiple path segments.

```html
<!-- Toggles the value of `$foo` -->
<div data-signals-foo="false">
  <button data-on-change="@toggleAll('foo')"></button>
</div>

<!-- Toggles the value of `$foo.bar.baz` -->
<div data-signals-foo.bar.baz="false">
  <button data-on-click="@toggleAll('foo.*.baz')"></button>
  <button data-on-click="@toggleAll('foo.**')"></button>
</div>
```

### `@fit()`

Arguments: `@fit(v: number, oldMin: number, oldMax: number, newMin: number, newMax: number, shouldClamp=false, shouldRound=false)`

Make a value linear interpolate from an original range to new one.