# Architecture Decision Record: Datastar SDK

## Summary

### Issue

Datastar has had a few helper tools in the past for different languages.  The SDK effort is to unify around the tooling needed for Hypermedia On Whatever your Like (HOWL) based UIs.  Although Datastar the library can use any plugins the default bundle includes robust Server Sent Event (SSE) base approach.  Most current languages and backend don't have great tooling around the style of delivering content to the frontend.

### Decision

Provide an SDK in a language agnostic way, to that end

1. Keep SDK as minimal as possible
2. Allow per language/framework extended features to live in an SDK ***sugar*** version

### Status

- [x] Create a document (this) to allow any one to make a spec compliant SDK for any language or framework
- [ ] Provide a [reference implementation](../code/go/sdkcore) in Go
- [ ] Provide SDKs for
  - [ ] JS/TS
  - [ ] PHP
  - [ ] .NET
  - [ ] Python
  - [ ] Java
  - [ ] Haskell?

## Details

### Assumptions

The core mechanics of Datastar's SSE support is

1. Data gets send to browser as SSE events
2. Data comes in via JSON from browser under a `datastar` namespace

### Constraints


# Library

> [!WARNING] All naming conventions are shown using `Go` as the standard, thing may change per language norms but please keep as close as possible.

## ServerSentEventGenerator

***There must*** be a `ServerSentEventGenerator` namespace.  In Go this is implemented as a struct, but could be a class or even namespace in languages such as C.

### Construction / Initialization
   1. ***There must*** be a way to create a new instance of this object based on the incoming `HTTP` Request and Response objects.
   2. The `ServerSentEventGenerator` ***must*** default to a flusher interface that has the following response headers set by default
      1. `Cache-Control = nocache`
      2. `Connection = keep-alive`
      3. `Content-Type = text/event-stream`
   3. Then the created response ***should*** `flush` immediately to avoid timeouts while 0-♾️ events are created
   4. `ServerSentEventGenerator` ***should*** include an incrementing number to be used as an id for events when one is not provided
   5. Multiple calls using `ServerSentEventGenerator` should be single threaded to guarantee order.  The Go implementation use a mutex to facilitate this behavior but might not be need in a some environments

### `ServerSentEventGenerator.send(eventType: EventType, dataLines: string[], options?: {id?:string, rery?:durationInMilliseconds})`

All top level `ServerSentEventGenerator` ***should*** use a unified sending function.  This method ***should be private***

####  Args

##### EventType
An enum of Datastar supported events.  Will be a string over the wire
Currently valid values are

| Event             | Description                                    |
|-------------------|------------------------------------------------|
| datastar-fragment | A fragment of HTML to be inserted into the DOM |
| datastar-signal   | Effect the data-store in some way              |
| datastar-remove   | Remove something from the DOM or data-store    |
| datastar-redirect | Redirect the browser to a new URL              |
| datastar-console  | Send a message to the browser console          |

##### Options
* `id` (string) Each event ***may*** include an `id`.  This can be used by the backend to replay events.  If one is not provided the server ***must*** include a monotonically incrementing id.  This is part of the SSE spec and is used to tell the browser how to handle the event.  For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
* `retry` (duration) Each event ***may*** include a `retry` value.  If one is not provided the SDK ***must*** default to `1 second`.  This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost. For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry

#### Logic
When called the function ***must*** write to the response buffer the following in specified order.  If any part of this process fails you ***must*** return/throw an error depending on language norms.
1.   ***Must*** write `event: EVENT_TYPE\n` where `EVENT_TYPE` is [EventType](#EventType)
2.   ***Must*** write `id: ID\n` where `ID` is either a user defined string or a monotonically increased integer starting at 0
3.   For each string in the provided `dataLines`, you ***must*** write `data: DATA\n` where `DATA` is the provided string.
4.  ***Must*** write a `\n\n` to complete the event per the SSE spec.
5.  Afterward the writer ***should*** immediately flush.  This can be confounded by other middlewares such as compression layers

### `ServerSentEventGenerator.RenderFragment(data: string, options ?: { selector?: string, merge ?: FragmentMergeMode, settleDuration?: durationInMilliseconds, useViewTransition?: boolean })`

`RenderFragment` is a helper function to send a fragment of HTML to the browser to be inserted into the DOM.

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
* `selector` (string) The CSS selector to use to insert the fragment.  If not provided the fragment ***must*** be inserted at the end of the body.  If the selector is not found Datastar client side ***will** default to using the `id` attribute of the fragment
* `merge` (FragmentMergeMode) The mode to use when merging the fragment into the DOM.  If not provided the Datastar client side ***will*** default to `morph`
* If `settleDuration` is provided the SDK ***should*** wait for the specified duration before sending the event, if not provided the Datastar client side ***will*** default to `300 milliseconds`
* If useViewTransition is provided the SDK ***should*** use the provided view transition, if not provided the Datastar client side ***will*** default to `false`

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `data` and `datastar-fragment` event type.
1. If the `selector` is provided the function ***must*** include the selector in the event data in the format `selector SELECTOR`, unless the selector is the id of the fragment
2. If the `merge` is provided the function ***must*** include the merge mode in the event data in the format `merge MERGE_MODE`, unless the merge mode is the default of `morph`.
3. If the `settleDuration` is provided the function ***must*** include the settle duration in the event data in the format `settleDuration: DURATION`, unless the settle duration is the default of `300 milliseconds`.
4. If the `useViewTransition` is provided the function ***must*** include the view transition in the event data in the format `useViewTransition VIEW_TRANSITION`, unless the view transition is the default of `false`.  `VIEW_TRANSITION` should be `true` or `false` depending on the value of the `useViewTransition` option.

### `ServerSentEventGenerator.RemoveFragments(seletor: string, options ?: { settleDuration?: durationInMilliseconds, useViewTransition?: boolean})`

`RemoveFragments` is a helper function to send a signal to the browser to remove a fragment from the DOM.

#### Args

`selector` is a CSS selector that represents the fragment to be removed from the DOM.  The selector ***must*** be a valid CSS selector.  The Datastar client side will use this selector to remove the fragment from the DOM.

#### Logic
1. When called the function ***must*** call `ServerSentEventGenerator.send` with the `data` and `datastar-remove` event type.
2. The function ***must*** include the selector in the event data in the format `selector SELECTOR`.
3. If the `settleDuration` is provided the function ***must*** include the settle duration in the event data in the format `settleDuration DURATION`, unless the settle duration is the default of `300 milliseconds`.  `DURATION` should be the provided duration in milliseconds.
4. If the `useViewTransition` is provided the function ***must*** include the view transition in the event data in the format `useViewTransition VIEW_TRANSITION`, unless the view transition is the default of `false`.  `VIEW_TRANSITION` should be `true` or `false` depending on the value of the `useViewTransition` option.


### `ServerSentEventGenerator.PatchStore(data: string, options ?: { onlyIfMissing?: boolean })`

`PatchStore` is a helper function to send a signal to the browser to update the data-store.

#### Args

Data is a JS or JSON object that will be sent to the browser to update the data-store.  The data ***must*** be a valid JS object.  Usually this will be in the form of a JSON string.  It will be converted to fine grain signals by the Datastar client side.

##### Options

* `onlyIfMissing` (boolean) If true the SDK ***should*** only send the signal if the data is not already in the data-store.  If not provided the Datastar client side ***will*** default to `false` which will cause the data to be merged into the data-store.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `data` and `datastar-signal` event type.

1. If the `onlyIfMissing` is provided the function ***must*** include the onlyIfMissing in the event data in the format `onlyIfMissing BOOLEAN`, unless the onlyIfMissing is the default of `false`.  `BOOLEAN` should be `true` or `false` depending on the value of the `onlyIfMissing` option.

### `ServerSentEventGenerator.RemoveFromStore(...paths: string[])`

`RemoveFromStore` is a helper function to send a signal to the browser to remove data from the data-store.

#### Args

`paths` is a list of strings that represent the path to the data to be removed from the data-store.  The paths ***must*** be valid `.` delimited paths within the store.  The Datastar client side will use these paths to remove the data from the data-store.

#### Logic
When called the function ***must*** call `ServerSentEventGenerator.send` with the `data` and `datastar-remove` event type.

1. The function ***must*** include the paths in the event data in the format `paths PATHS` where `PATHS` is a space separated list of the provided paths.


### `ServerSentEventGenerator.Redirect(url: string)`

#### Args

`url` is a string that represents the URL to redirect the browser to.  The URL ***must*** be a valid URL.  The Datastar client side will use this URL to redirect the browser.

#### Logic
1. When called the function ***must*** call `ServerSentEventGenerator.send` with the `data` and `datastar-redirect` event type.
2. The function ***must*** include the URL in the event data in the format `url URL` where `URL` is the provided URL.

### `ServerSentEventGenerator.Console(mode: ConsoleMode, message: string)`

`Console` allows developers to send messages directly to a browser console

#### Args

##### ConsoleMode

An enum of Datastar supported console modes.  Will be a string over the wire
Valid values should match the [ConsoleAPI](https://developer.mozilla.org/en-US/docs/Web/API/console) methods and currently include

* assert
* clear
* count
* countReset
* debug
* dir
* dirxml
* error
* group
* groupCollapsed
* groupEnd
* info
* log
* table
* time
* timeEnd
* timeLog
* trace
* warn

## `ParseIncoming(r *http.Request, store any) error`

`ParseIncoming` is a helper function to parse incoming data from the browser.  It should take the incoming request and convert into an object that can be used by the backend.

#### Args

* `r` (http.Request) The incoming request object from the browser.  This object ***must*** be a valid Request object per the language specifics.
* `store` (any) The store object that will the incoming data will be unmarshalled into.  The exact function signature will depend on the language specifics.

#### Logic

1. The function ***must*** parse the incoming HTTP request
   1. if the incoming method is `GET` then the function ***must*** parse the query string's `datastar` key and treat it as a URL encoded JSON string.
   2. otherwise the function ***must*** parse the body of the request as a JSON encoded string.
   3. if the incoming data is not valid JSON the function ***must*** return an error.
