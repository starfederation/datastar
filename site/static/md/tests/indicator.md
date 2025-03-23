# Indicator

Tests that the indicator signal is set to `false` when a request completes.

<div>
  <div id="content" data-signals-result="0"></div>
  <button id="clickable" data-on-click="@get('/tests/indicator_element_removed/data')" data-indicator-fetching class="btn">Fetch</button>
  <hr />
  Result:
  <code id="result" data-text="$result && !$fetching ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>