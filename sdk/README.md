# Architecture Decision Record: Datastar SDK

## Summary

Datastar SDK provides unified tooling for building Hypermedia On Whatever you Like (HOWL) based UIs across multiple languages. While Datastar supports various plugins, the default bundle focuses on a robust Server-Sent Event (SSE) approach, addressing the lack of good SSE tooling in most languages and backends.

## Decision

Provide a language-agnostic SDK with these principles:

1. **Minimal Core**: Keep the SDK as minimal as possible
2. **Sugar Extensions**: Allow per-language/framework extended features in SDK "sugar" versions

### Naming Rationale

**Why "Patch" instead of "Merge":**
The prefix "Patch" was chosen to better reflect the non-idempotent nature of these operations. Unlike PUT requests that replace entire resources, PATCH requests apply partial modifications. This aligns with our SDK's behavior where operations modify specific parts of the DOM or signal state rather than replacing them entirely.

**Why "Elements" instead of "Fragments":**
We use "Elements" because it accurately describes what the SDK handles - complete HTML elements, not arbitrary DOM nodes like text nodes or document fragments. This naming matches the actual intent and constraints of the system, making the API clearer and more predictable for developers.

## Details

### Core Mechanics

The core mechanics of Datastar’s SSE support is

1. **Server → Browser**: Data is sent as SSE events
2. **Browser → Server**: Data arrives as JSON under the `datastar` namespace

# SDK Specification

> [!WARNING] 
> All naming conventions use Go as the reference implementation. Adapt to language-specific conventions while maintaining consistency.

## ServerSentEventGenerator

**Required**: A `ServerSentEventGenerator` namespace/class/struct (implementation varies by language).

---

### Construction / Initialization

**Requirements:**

| Requirement | Description |
|-------------|-------------|
| **Constructor** | ***Must*** accept HTTP Request and Response objects |
| **Response Headers** | ***Must*** set:<br>• `Cache-Control: no-cache`<br>• `Content-Type: text/event-stream`<br>• `Connection: keep-alive` (HTTP/1.1 only - [see spec](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Connection)) |
| **Immediate Flush** | ***Should*** flush response immediately to prevent timeouts |
| **Thread Safety** | ***Should*** ensure ordered delivery (e.g., mutex in Go) |

---

### `ServerSentEventGenerator.send`

```typescript
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
```

A unified sending function ***should*** be used internally (private/protected).

#### Parameters

##### EventType

String enum of supported events:

| Event | Description |
|-------|-------------|
| `datastar-patch-elements` | Patches HTML elements into the DOM |
| `datastar-patch-signals` | Patches signals into the signal store |

##### Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `eventId` | string | - | Unique event identifier for replay functionality ([SSE spec](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id)) |
| `retryDuration` | ms | `1000` | Reconnection delay after connection loss ([SSE spec](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry)) |

#### Implementation Requirements

***Must*** write to response buffer in this exact order:

1. `event: EVENT_TYPE\n`
2. `id: EVENT_ID\n` (if eventId provided)
3. `retry: RETRY_DURATION\n` (***unless*** default 1000ms)
4. `data: DATA\n` (for each dataLine)
5. `\n` (end of event)
6. ***Should*** flush immediately (note: compression middleware may interfere)

**Error Handling**: ***Must*** return/throw errors per language conventions.

---

### `ServerSentEventGenerator.PatchElements`

```typescript
ServerSentEventGenerator.PatchElements(
    elements: string,
    options?: {
        selector?: string,
        mode?: ElementPatchMode,
        useViewTransition?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
```

#### Example Output

<details>
<summary>Minimal Example</summary>

```http
event: datastar-patch-elements
data: elements <div id="feed"><span>1</span></div>

```
</details>

<details>
<summary>Full Example (all options)</summary>

```http
event: datastar-patch-elements
id: 123
retry: 2000
data: mode inner
data: selector #feed
data: useViewTransition true
data: elements <div id="feed">
data: elements     <span>1</span>
data: elements </div>

```
</details>

<details>
<summary>Patch elements based on their id</summary>

```
event: datastar-patch-elements
data: elements <div id="id1">New content.</div>
data: elements <div id="id2">Other new content.</div>
```
</details>

<details>
<summary>Insert a new element based on a selector</summary>

```
event: datastar-patch-elements
data: mode append
data: selector #mycontainer
data: elements <div>New content</div>
```
</details>

<details>
<summary>Remove elements based on a selector</summary>

```
event: datastar-patch-elements
data: mode remove
data: selector #feed, #otherid
```
</details>

<details>
<summary>Remove elements without a selector</summary>

```
event: datastar-patch-elements
data: mode remove
data: elements <div id="first"></div><div id="second"></div>
```
</details>

`PatchElements` sends HTML elements to the browser for DOM manipulation.

> [!TIP]
> - To remove elements, use the `remove` merge mode
> - To execute JavaScript, send a `<script>` element - it will auto-execute when added to the DOM

### Elements vs Fragments: Key Distinction

> [!IMPORTANT]
> Datastar requires **complete HTML elements**, not fragments.

| Approach | Example | Characteristics |
|----------|---------|-----------------|
| **Datastar (Elements)** | `<div id="content">Hello</div>` | • Complete, well-formed HTML<br>• Valid opening/closing tags<br>• Standard DOM API compatible<br>• Predictable browser behavior |
| **HTMX (Fragments)** | `Hello <strong>World</strong>` | • Partial HTML allowed<br>• May lack proper structure<br>• Requires special handling<br>• More flexible but less predictable |

### Parameters

- **elements**: One or more complete HTML elements. If a selector has not been specified, each element must contain an
  id. With ElementPatchMode remove, this parameter may be omitted when a selector is supplied.

#### ElementPatchMode

String enum defining how elements are patched into the DOM.

##### Available Modes

| Mode |Is Morphed? | Description |
|------|------|-------------|
| `outer` | ✔️ | Morph entire element, preserving state |
| `inner` | ✔️ | Morph inner HTML only, preserving state |
| `replace` | 🚫 | Replace entire element, reset state |
| `prepend` | 🚫 | Insert at beginning inside target |
| `append` | 🚫 | Insert at end inside target |
| `before` | 🚫 | Insert before target element |
| `after` | 🚫 | Insert after target element |
| `remove` | 🚫 | Remove target element from DOM |

#### Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `selector` | string | Element's `id` | CSS selector for target element. If a selector is not specified, each element must have an id specified. |
| `mode` | ElementPatchMode | `outer` | How to patch the element |
| `useViewTransition` | boolean | `false` | Enable view transitions API |

### Implementation

***Must*** call `ServerSentEventGenerator.send` with event type `datastar-patch-elements`.

**Data format** (only include non-defaults):
- `selector SELECTOR\n` (if provided)
- `mode PATCH_MODE\n` (if not `outer`)
- `useViewTransition true\n` (if `true`)
- `elements HTML_LINE\n` (for each line of HTML)

---


### `ServerSentEventGenerator.PatchSignals`

```typescript
ServerSentEventGenerator.PatchSignals(
    signals: string,
    options ?: {
        onlyIfMissing?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
```

#### Example Output

<details>
<summary>Minimal Example</summary>

```http
event: datastar-patch-signals
data: signals {"output":"Patched Output Test","show":true,"input":"Test","user":{"name":"","email":""}}

```
</details>

<details>
<summary>Full Example (all options)</summary>

```http
event: datastar-patch-signals
id: 123
retry: 2000
data: onlyIfMissing true
data: signals {"output":"Patched Output Test","show":true,"input":"Test","user":{"name":"","email":""}}

```
</details>

`PatchSignals` sends signals to the browser using [RFC 7386 JSON Merge Patch](https://datatracker.ietf.org/doc/html/rfc7386) semantics.

### Parameters

- **signals**: Valid JSON string containing the patch data

### RFC 7386 JSON Merge Patch Behavior

| Operation | Behavior | Example |
|-----------|----------|---------|
| **Add/Update** | Set property value | `{"key": "value"}` |
| **Remove** | Set to `null` | `{"key": null}` |
| **Nested** | Recursive merge | `{"user": {"name": "John"}}` |

### Examples

<details>
<summary>Signal Operations Examples</summary>

```json
// Add signal
{"newSignal": "value"}

// Update signal
{"existingSignal": "newValue"}

// Remove signal
{"signalToRemove": null}

// Complex nested merge
{
  "user": {
    "name": "John",
    "email": null,
    "preferences": {
      "theme": "dark"
    }
  }
}
// Result:
// - user.name = "John"
// - user.email removed
// - user.preferences.theme = "dark"
```
</details>

### Options

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `onlyIfMissing` | boolean | `false` | Only patch if signal doesn't exist |

### Implementation

***Must*** call `ServerSentEventGenerator.send` with event type `datastar-patch-signals`.

**Data format**:
- `onlyIfMissing true\n` (only if `true`)
- `signals JSON_LINE\n` (for each line of JSON)

---

## `ReadSignals`

`ReadSignals` parses incoming signal data from the browser into backend objects.

```go
ReadSignals(request *http.Request, signals any) error
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `request` | HTTP Request | Language-specific request object |
| `signals` | any | Target object/struct for unmarshaling |

### Implementation

The function ***must*** parse the incoming HTTP request based on the method:

| Method | Data Location | Format | Description |
|--------|---------------|--------|-------------|
| `GET` | Query parameter `datastar` | URL-encoded JSON | Extract from query string |
| Others | Request body | JSON | Parse request body directly |

**Error Handling**: ***Must*** return error for invalid JSON.
