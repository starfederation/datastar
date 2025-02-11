## Merge Refs

Tests that `data-ref` is correctly applied after merging.

<div>
  <div id="content"><button data-ref-result data-on-click="@get('/tests/merge_refs/data')" data-val="0" class="btn">Merge</button></div>
  <hr />
  Result:
  <code id="result" data-text="$result.getAttribute('data-val')"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>