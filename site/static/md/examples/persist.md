## Demo

<div data-signals="{nested: {test1: 'foo', test2: 'bar', test3: 'baz'}}" data-persist-foo="nested.test1 nested.test3">
  <input id="keyInput" class="input input-bordered" data-bind="nested.test1" />
  <br>
  <input id="keyInput" class="input input-bordered" data-bind="nested.test2" />
  <br>
  <input id="keyInput" class="input input-bordered" data-bind="nested.test3" />
  <pre data-text="ctx.signals.JSON()">Replace me</pre>
</div>

## Explanation

```html
<div data-signals="{nested: {test1: 'foo', test2: 'bar', test3: 'baz'}}" data-persist-foo="nested.test1 nested.test3">
  <input class="input input-bordered" data-bind="nested.test1" />
  <input class="input input-bordered" data-bind="nested.test2" />
  <input class="input input-bordered" data-bind="nested.test3" />
  <pre data-text="ctx.signals.JSON()">Replace me</pre>
</div>
```

Look at your Local Storage in your browser's developer tools.

In this example we are caching the `nested.test1` and `nested.test3` values in the Local Storage.

If you don't use any values it will cache the entire signals.