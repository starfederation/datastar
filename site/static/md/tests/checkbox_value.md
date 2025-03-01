# Checkbox Value

Tests that a checkbox input's bound signal is set to an empty string when unchecked and its value when checked.

<div>
  <input id="clickable" type="checkbox" data-bind-result value="foo" />
  <span data-text="$result"></span>
  <hr />
  Result:
  <code id="result" data-text="$result === 'foo' ? 1 : ($result === '' ? 0 : -1)"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>