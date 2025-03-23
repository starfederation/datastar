# Merge Fragment On Load

Tests merging a fragment containing `data-on-load`.

<div>
  <div id="content" data-signals-result="0" data-on-load="$result = 0"></div>
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_on_load/data')" class="btn">Merge</button>
  <hr />
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>