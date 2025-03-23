# Merge Fragment Signals

Tests merging a fragment containing `data-signals-*`.

<div>
  <div id="content" data-signals-result="0"></div>
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_signals/data')" class="btn">Merge</button>
  <hr />
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>