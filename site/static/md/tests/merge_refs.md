## Merge Refs

`data-ref` is correctly applied after merging.

<div id="content">
  Input:
  <button data-ref-foo data-on-click="@get('/tests/merge_refs/data')" class="btn">foo</button>
  <hr />
  Output:
  <code id="result" data-text="$foo.innerHTML"></code>
  <hr />
  Expected result: <code>foo</code>
</div>