# Plugin Name Prefix

Tests that data attributes with plugin names in their prefix are not processed. In this test, `data-classxyz` should be ignored.

<div data-classxyz="whatever">
  Result:
  <code id="result" data-text="1">0</code>
  <hr />
  Expected result on load: <code>1</code>
</div>