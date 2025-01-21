## Demo

<div data-signals="{namespace: {test1: 'foo', test2: 'bar', test3: 'baz'}}" data-persist-foo="namespace.test1 namespace.test3">
    <input id="keyInput" class="input input-bordered" data-bind="namespace.test1"/>
    <br>
    <input id="keyInput" class="input input-bordered" data-bind="namespace.test2"/>
    <br>
    <input id="keyInput" class="input input-bordered" data-bind="namespace.test3"/>
    <pre data-text="ctx.signals.JSON()">Replace me</pre>
</div>

## Explanation

```html
<div
  data-signals="{namespace: {test1: 'foo', test2: 'bar', test3: 'baz'}}"
  data-persist-foo="namespace.test1 namespace.test3"
>
  <input class="input input-bordered" data-bind="namespace.test1" />
  <input class="input input-bordered" data-bind="namespace.test2" />
  <input class="input input-bordered" data-bind="namespace.test3" />
  <pre data-text="ctx.signals.JSON()">Replace me</pre>
</div>
```

Look at your Local Storage in your browser's developer tools.

In this example we are caching the `namespace.test1` and `namespace.test3` values in the Local Storage.

If you don't use any values it will cache the entire signals.