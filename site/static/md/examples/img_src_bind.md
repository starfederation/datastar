## Demo

<div
    id="file_upload"
    data-signals="{id: 237, min: 1, max: 1024}"
>
    <img class="rounded ring-4 ring-accent" data-attr-src="`https://picsum.photos/id/${id.value}/640/320`" />
    <input class="input input-bordered" type="number" step="1"  data-attr="{min: min.value, max: max.value}" data-bind-id>
    <button class="btn btn-primary" data-on-click="id.value = fit(Math.random(), 0, 1, min.value, max.value, true, true)">Random</button>
</div>

## Explanation

```html
<div id="file_upload" data-signals="{id: 237, min: 1, max: 1024}">
  <img data-attr-src="`https://picsum.photos/id/${id.value}/640/320`" />
  <input
    type="number"
    step="1"
    data-on-click="sse('arstasrt', {method: 'post'})"
    data-attr="{min: min.value, max: max.value}"
    data-bind-id
  />
  <button data-on-click="id.value = fit(Math.random(), 0, 1, min.value, max.value, true, true)">
    Random
  </button>
</div>
```

A discussion on the Discord channel found a bug in binding images. This is an example to make sure it works going forward. The binding of the `src` attribute to a template literal allows for a dynamic url and is hitting the picsum backend, not ours so no HTML fragments are needed.

The `@fitInt` actions is a nice helper to simplify picking a random valid number.
