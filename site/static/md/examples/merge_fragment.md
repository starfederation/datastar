## Merge Fragment

## Demo

<div id="example">
  <div id="fragment" data-signals-foo="'bar'">
    <div data-text="$foo"></div>
    <button data-on-click="@get('/examples/merge_fragment/data')" class="btn btn-primary">Reload</button>
  </div>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>
