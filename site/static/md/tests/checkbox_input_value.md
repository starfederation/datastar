# Checkbox Input Value

Tests that a checkbox input's value is assigned to a bound signal when checked.

<div>
  <input id="clickable" type="checkbox" data-bind-result value="1" />
  <hr />
  Result:
  <code id="result" data-text="$result ? $result : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>