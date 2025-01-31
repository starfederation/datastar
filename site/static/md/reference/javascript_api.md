# JavaScript API

Datastar is intentional about not (indecently) exposing itself in the global scope – you _should_ be able to do everything you need via Datastar expressions in [attribute plugins](/reference/attribute_plugins) and [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE events.

When troubleshooting an issue, it may be useful to see the current state of the signals. The easiest way to do this is by outputting them in JSON format using `data-text`.

```html
<div data-text="ctx.signals.JSON()"></div>
```

While it is generally recommended against, you can manually import the Datastar object and access its public methods and properties.

```html
<script type="module">
    import { Datastar } from '/path/to/datastar.js'
    
    Datastar.load()
</script>
```

## Public Methods

The Datastar object exposes the following methods.

### `load()`

Loads all plugins and applies them to the DOM.

```js
Datastar.load()
```

## Public Properties

The Datastar object exposes the following properties.

### `signals`

The signal root, on which you can access signal methods. Beware that you should avoid using this for anything other than troubleshooting.

```js
Datastar.signals.values()
```