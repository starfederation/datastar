# On Load

Tests that the on load event is triggered after all other data attributes on the element are loaded, without forcing the
user to specify the correct order.

<div data-on-load="@get('/tests/on_load/data')" data-signals-fetching="true">
  Result:
  <code id="result">0</code>
  <hr />
  Expected result on load: <code>1</code>
</div>