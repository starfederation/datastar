## Merge Refs

Tests that `data-ref` is correctly applied after merging.

<div>
  <div id="content"><button data-ref-foo data-on-click="@get('/tests/merge_refs/data')" class="btn">foo</button></div>
  <hr />
  Result:
  <code id="result" data-text="$foo.innerHTML"></code>
  <hr />
  Expected result on click: <code>bar</code>
</div>