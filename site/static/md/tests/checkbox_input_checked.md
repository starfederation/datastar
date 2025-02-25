# Checkbox Input Checked

Tests that a checkbox input's bound signal is initially set to its value when initially checked.

<div>
  <input id="clickable" type="checkbox" data-bind-result value="foo" checked />
  <span data-text="$result"></span>
  <hr />
  Result:
  <code id="result" data-text="$result === 'foo' ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>