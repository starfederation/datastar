## Merge Fregment Signal

Tests that merging a fragment containing `data-signals-*` works.

<div>
  <div id="content" data-signals-foo="1"><button data-on-click="@get('/tests/merge_fragment_signal/data')" class="btn btn-primary">Reload</button></div>
  <hr />
  Result:
  <code id="result" data-text="$foo"></code>
  <hr />
  Expected result on click: <code>2</code>
</div>