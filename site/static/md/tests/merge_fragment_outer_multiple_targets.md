# Merge Fragment Outer Multiple Targets

Tests merging a fragment using the `outer` merge mode with multiple targets.

<div>
  <div id="content" data-signals-result="0"></div>
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_outer_multiple_targets/data')" class="btn">Merge</button>
  <div class="target"></div>
  <div class="target"></div>
  <hr />
  Result:
  <code id="result" data-text="$result == 2 ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>