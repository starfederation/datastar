# Checkbox Boolean Checked

Tests that a checkbox input's bound signal is set to `false` when unchecked and `true` when checked.

<div>
  <input id="clickable" type="checkbox" data-bind-result checked /> 
  <span data-text="$result"></span>
  <hr />
  Result:
  <code id="result" data-text="$result === false ? 1 : ($result === true ? 0 : -1)"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>