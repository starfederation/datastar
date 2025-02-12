## Signals Change

## Demo

<div
  data-signals="{clicks:0, _localState: {bar: 1234}, _anotherLocalVar: 'hello'}"
  data-on-signals-change="@post('/examples/signals_changed/updates')"
  >
    <div class="flex flex-col md:flex-row gap-4">
      <button
        id="increment"
        class="btn btn-success"
        data-on-click="$clicks++"
        >Click Me</button>
      <button
        id="clear"
        class="btn btn-warning"
        data-on-click="$clicks=0; @delete('/examples/signals_changed/updates')"
      >Clear Local & Server</button>
      <button
        id="reload"
        class="btn btn-error"
        data-on-click="window.location.reload()"
      >Reload Page</button>
    </div>
    <div id="local_clicks">Local Clicks: <span data-text="$clicks"></span></div>
    <div id="from_server"></div>
    <pre data-text="ctx.signals.JSON()"></pre>
</div>

## Explanation

```html
<div
  data-signals="{clicks:0, _localState: {bar: 1234}, _anotherLocalVar: 'hello'}"
  data-on-signals-change="@post('/examples/signals_changed/updates')"
>
  <div>
    <button id="increment" data-on-click="$clicks++">Click Me</button>
    <button
      id="clear"
      data-on-click="$clicks=0; @delete('/examples/signals_changed/updates')"
    >
      Clear Local & Server
    </button>
    <button id="reload" data-on-click="window.location.reload()">
      Reload Page
    </button>
  </div>
  <div>Local Clicks: <span data-text="$clicks"></span></div>
  <div id="from_server"></div>
</div>
```

`data-on-signals-change` is a special event that is triggered when the signals changes. This is useful for updating the UI when the signals changes. In this example we update the `clicks` signals with a new value. This triggers a re-render of the `clicks` span element. You can still use the `__throttle` and `__debounce` modifiers to control the rate of updates even further. In this case we are sending the signals changes to the server to update the lifetime total clicks the server has seen.


```json
{
  "_sidebarOpen": false,
  "clicks": 0,
  "_localState": {
    "bar": 1234
  },
  "_anotherLocalVar": "hello"
}
```

Whereas if you look at the Network tab in the browser you should see the following request payload

```json
{ "clicks": 0 }
```

Any signal (or namespaced set of signals) starting with an underscore `_` is considered local and will not be sent to the server. In this example `_localState` and `_anotherLocalVar` are local only.
