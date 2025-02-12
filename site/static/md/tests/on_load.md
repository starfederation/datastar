# On Load

Tests that the on load event is triggered after all other data attributes on the element are loaded, without forcing the user to specify the correct order.

<div data-signals-result="0" data-on-signals-change-fetching="$result = 1" data-on-load="@get('/tests/on_load/data')" data-indicator="$fetching">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>