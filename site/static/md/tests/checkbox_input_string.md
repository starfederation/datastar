# Checkbox Input String

Tests that a checkbox input's bound signal string is set to its value when checked.

<div data-signals-result="''">
  <input id="clickable" type="checkbox" data-bind-result value="foo" />
  <span data-text="$result"></span>
  <hr />
  Result:
  <code id="result" data-text="$result === 'foo' ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>