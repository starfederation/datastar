# Architecture Decision Record: Datastar SDK

## Summary

Datastar has had a few helper tools in the past for different languages.  The SDK effort is to unify around the tooling needed for Hypermedia On Whatever your Like (HOWL) based UIs.  Although Datastar the library can use any plugins the default bundle includes robust Server Sent Event (SSE) base approach.  Most current languages and backend don't have great tooling around the style of delivering content to the frontend.

### Decision

Provide an SDK in a language agnostic way, to that end

1. Keep SDK as minimal as possible
2. Allow per language/framework extended features to live in an SDK ***sugar*** version

### Status

- [x] Create a document (this) to allow any one to make a spec compliant SDK for any language or framework
- [x] Provide a [reference implementation](../sdk/go) in Go
- [ ] Provide SDKs for
  - [ ] JS/TS
  - [x] PHP
  - [x] .NET
  - [x] Python
  - [x] Java
  - [ ] Haskell?

## Details

### Assumptions

The core mechanics of Datastar's SSE support is

1. Data gets sent to browser as SSE events.
2. Data comes in via JSON from browser under a `datastar` namespace.

# Library

> [!WARNING] All naming conventions are shown using `Go` as the standard, thing may change per language norms but please keep as close as possible.

## ServerSentEventGenerator

***There must*** be a `ServerSentEventGenerator` namespace.  In Go this is implemented as a struct, but could be a class or even namespace in languages such as C.

### Construction / Initialization
   1. ***There must*** be a way to create a new instance of this object based on the incoming `HTTP` Request and Response objects.
   2. The `ServerSentEventGenerator` ***must*** use a response controller that has the following response headers set by default
      1. `Cache-Control = nocache`
      2. `Content-Type = text/event-stream`
      3. `Connection = keep-alive` ***only*** if a HTTP/1.1 connection is used (see [spec](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Connection))
   3. Then the created response ***should*** `flush` immediately to avoid timeouts while 0-♾️ events are created
   4. Multiple calls using `ServerSentEventGenerator` should be single threaded to guarantee order.  The Go implementation use a mutex to facilitate this behavior but might not be need in a some environments

### `ServerSentEventGenerator.send`

```
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
```

All top level `ServerSentEventGenerator` ***should*** use a unified sending function.  This method ***should be private/protected***

####  Args

##### EventType
An enum of Datastar supported events.  Will be a string over the wire.
Currently valid values are

| Event                     | Description                         |
|---------------------------|-------------------------------------|
| datastar-merge-fragments  | Merges HTML fragments into the DOM  |
| datastar-merge-signals    | Merges signals into the signals       |
| datastar-remove-fragments | Removes HTML fragments from the DOM |
| datastar-remove-signals   | Removes signals from the signals      |
| datastar-execute-script   | Executes JavaScript in the browser  |

##### Options
* `eventId` (string) Each event ***may*** include an `eventId`.  This can be used by the backend to replay events.  This is part of the SSE spec and is used to tell the browser how to handle the event.  For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
* `retryDuration` (duration) Each event ***may*** include a `retryDuration` value.  If one is not provided the SDK ***must*** default to `1000` milliseconds.  This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost. For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry

#### Logic
When called the function ***must*** write to the response buffer the following in specified order.  If any part of this process fails you ***must*** return/throw an error depending on language norms.
1. ***Must*** write `event: EVENT_TYPE\n` where `EVENT_TYPE` is [EventType](#EventType)
2. If a user defined event ID is provided, the function ***must*** write `id: EVENT_ID\n` where `EVENT_ID` is the event ID.
3. ***Must*** write `retry: RETRY_DURATION\n` where `RETRY_DURATION` is the provided retry duration, ***unless*** the value is the default of `1000` milliseconds.
4. For each string in the provided `dataLines`, you ***must*** write `data: DATA\n` where `DATA` is the provided string.
5. ***Must*** write a `\n\n` to complete the event per the SSE spec.
6. Afterward the writer ***should*** immediately flush.  This can be confounded by other middlewares such as compression layers

### `ServerSentEventGenerator.MergeFragments`

```
ServerSentEventGenerator.MergeFragments(
    fragments: string,
    options?: {
        selector?: string,
        mergeMode?: FragmentMergeMode,
        settleDuration?: durationInMilliseconds,
        useViewTransition?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
```

#### Example Output

```
event: datastar-merge-fragments
id: 123
retryDuration: 2000
data: selector #feed
data: settleDuration 10
data: useViewTransition true
data: fragments <div id="feed">
data: fragments     <span>1</span>
data: fragments </div>
```

`MergeFragments` is a helper function to send HTML fragments to the browser to be merged into the DOM.

#### Args

##### FragmentMergeMode

An enum of Datastar supported fragment merge modes.  Will be a string over the wire
Valid values should match the [FragmentMergeMode](#FragmentMergeMode) and currently include

| Mode             | Description                                             |
|------------------|---------------------------------------------------------|
| morph            | Use idiomorph to merge the fragment into the DOM        |
| inner            | Replace the innerHTML of the selector with the fragment |
| outer            | Replace the outerHTML of the selector with the fragment |
| prepend          | Prepend the fragment to the selector                    |
| append           | Append the fragment to the selector                     |
| before           | Insert the fragment before the selector                 |
| after            | Insert the fragment after the selector                  |
| upsertAttributes | Update the attributes of the selector with the fragment |

##### Options
* `selector` (string) The CSS selector to use to insert the fragments.  If not provided or empty, Datastar **will** default to using the `id` attribute of the fragment.
* `mergeMode` (FragmentMergeMode) The mode to use when merging the fragment into the DOM.  If not provided the Datastar client side ***will*** default to `morph`.
* `settleDuration` is used to control the amount of time that a fragment should take before removing any CSS related to settling.  It is used to allow for animations in the browser via the Datastar client.  If provided the value ***must*** be a positive integer of the number of milliseconds to allow for settling.  If none is provided, the default value of `300` milliseconds will be used.
* `useViewTransition` Whether to use view transitions, if not provided the Datastar client side ***will*** default to `false`.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `datastar-merge-fragments` event type.
1. If `selector` is provided, the function ***must*** include the selector in the event data in the format `selector SELECTOR\n`, ***unless*** the selector is empty.
2. If `mergeMode` is provided, the function ***must*** include the merge mode in the event data in the format `merge MERGE_MODE\n`, ***unless*** the value is the default of `morph`.
3. If `settleDuration` is provided, the function ***must*** include the settle duration in the event data in the format `settleDuration SETTLE_DURATION\n`, ***unless*** the value is the default of `300` milliseconds.
4. If `useViewTransition` is provided, the function ***must*** include the view transition in the event data in the format `useViewTransition USE_VIEW_TRANSITION\n`, ***unless*** the value is the default of `false`.  `USE_VIEW_TRANSITION` should be `true` or `false` (string), depending on the value of the `useViewTransition` option.
5. The function ***must*** include the fragments in the event data, with each line prefixed with `fragments `. This ***should*** be output after all other event data.

### `ServerSentEventGenerator.RemoveFragments`

```
ServerSentEventGenerator.RemoveFragments(
    selector: string,
    options?: {
        settleDuration?: durationInMilliseconds,
        useViewTransition?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
```

#### Example Output

```
event: datastar-remove-fragments
id: 123
retryDuration: 2000
data: selector #target
data: settleDuration 200
data: useViewTransition true
```

`RemoveFragments` is a helper function to send a selector to the browser to remove HTML fragments from the DOM.

#### Args

`selector` is a CSS selector that represents the fragments to be removed from the DOM.  The selector ***must*** be a valid CSS selector.  The Datastar client side will use this selector to remove the fragment from the DOM.

##### Options

* `settleDuration` is used to control the amount of time that a fragment should take before removing any CSS related to settling.  It is used to allow for animations in the browser via the Datastar client.  If provided the value ***must*** be a positive integer of the number of milliseconds to allow for settling.  If none is provided, the default value of `300` milliseconds will be used.
* `useViewTransition` Whether to use view transitions, if not provided the Datastar client side ***will*** default to `false`.

#### Logic
1. When called the function ***must*** call `ServerSentEventGenerator.send` with the `datastar-remove-fragments` event type.
2. The function ***must*** include the selector in the event data in the format `selector SELECTOR\n`.
3. If `settleDuration` is provided, the function ***must*** include the settle duration in the event data in the format `settleDuration SETTLE_DURATION\n`, ***unless*** the value is the default of `300` milliseconds.
4. If `useViewTransition` is provided, the function ***must*** include the view transition in the event data in the format `useViewTransition USE_VIEW_TRANSITION\n`, ***unless*** the value is the default of `false`.  `USE_VIEW_TRANSITION` should be `true` or `false` (string), depending on the value of the `useViewTransition` option.


### `ServerSentEventGenerator.MergeSignals`

```
ServerSentEventGenerator.MergeSignals(
    signals: string,
    options ?: {
        onlyIfMissing?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
```

#### Example Output

```
event: datastar-merge-signals
id: 123
retryDuration: 2000
data: onlyIfMissing true
data: signals {"output":"Patched Output Test","show":true,"input":"Test","user":{"name":"","email":""}}
```

`MergeSignals` is a helper function to send one or more signals to the browser to be merged into the signals.

#### Args

Data is a JavaScript object or JSON string that will be sent to the browser to update signals in the signals.  The data ***must*** evaluate to a valid JavaScript.  It will be converted to signals by the Datastar client side.

##### Options

* `onlyIfMissing` (boolean) Whether to merge the signal only if it does not already exist.  If not provided, the Datastar client side ***will*** default to `false`, which will cause the data to be merged into the signals.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `datastar-merge-signals` event type.

1. If `onlyIfMissing` is provided, the function ***must*** include it in the event data in the format `onlyIfMissing ONLY_IF_MISSING\n`, ***unless*** the value is the default of `false`.  `ONLY_IF_MISSING` should be `true` or `false` (string), depending on the value of the `onlyIfMissing` option.
2. The function ***must*** include the signals in the event data, with each line prefixed with `signals `.  This ***should*** be output after all other event data.

### `ServerSentEventGenerator.RemoveSignals`

```html
ServerSentEventGenerator.RemoveSignals(
    paths: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
```

#### Example Output

```
event: datastar-remove-signals
id: 123
retryDuration: 2000
data: paths user.name
data: paths user.email
```

`RemoveSignals` is a helper function to send signals to the browser to be removed from the signals.

#### Args

`paths` is a list of strings that represent the signal paths to be removed from the signals.  The paths ***must*** be valid `.` delimited paths to signals within the signals.  The Datastar client side will use these paths to remove the data from the signals.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `datastar-remove-signals` event type.

1. The function ***must*** include the paths in the event data, with each line prefixed with `paths `.  Space-separated paths such as `paths foo.bar baz.qux.hello world` ***should*** be allowed.

### `ServerSentEventGenerator.ExecuteScript`

```
ServerSentEventGenerator.ExecuteScript(
    script: string,
    options?: {
        autoRemove?: boolean,
        attributes?: string,
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
```

#### Example Output

```
event: datastar-execute-script
id: 123
retryDuration: 2000
data: autoRemove false
data: attributes type text/javascript
data: script window.location = "https://data-star.dev"
```

#### Args

`script` is a string that represents the JavaScript to be executed by the browser.

##### Options

* `autoRemove` Whether to remove the script after execution, if not provided the Datastar client side ***will*** default to `true`.
* `attributes` A line separated list of attributes to add to the `script` element, if not provided the Datastar client side ***will*** default to `type module`. Each item in the array should be a string in the format `key value`.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `datastar-execute-script` event type.

1. If `autoRemove` is provided, the function ***must*** include the auto remove script value in the event data in the format `autoRemove AUTO_REMOVE\n`, ***unless*** the value is the default of `true`.
2. If `attributes` is provided, the function ***must*** include the attributes in the event data, with each line prefixed with `attributes `, ***unless*** the attributes value is the default of `type module`.
3. The function ***must*** include the script in the event data, with each line prefixed with `script `.  This ***should*** be output after all other event data.

## `ReadSignals(r *http.Request, signals any) error`

`ReadSignals` is a helper function to parse incoming data from the browser.  It should take the incoming request and convert into an object that can be used by the backend.

#### Args

* `r` (http.Request) The incoming request object from the browser.  This object ***must*** be a valid Request object per the language specifics.
* `signals` (any) The signals object that will the incoming data will be unmarshalled into.  The exact function signature will depend on the language specifics.

#### Logic

1. The function ***must*** parse the incoming HTTP request
   1. If the incoming method is `GET`, the function ***must*** parse the query string's `datastar` key and treat it as a URL encoded JSON string.
   2. Otherwise, the function ***must*** parse the body of the request as a JSON encoded string.
   3. If the incoming data is not valid JSON, the function ***must*** return an error.
