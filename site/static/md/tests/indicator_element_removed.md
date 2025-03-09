# Indicator Element Removed

Tests that the indicator signal is set to `false` when the element it was on is removed from the DOM.

<div>
  <div id="content" data-signals-result="0"><button id="clickable" data-on-click="@get('/tests/indicator_element_removed/data')" data-indicator-fetching class="btn">Fetch</button></div>
  <hr />
  Result:
  <code id="result" data-text="$result && !$fetching ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>