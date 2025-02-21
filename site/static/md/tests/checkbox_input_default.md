# Checkbox Input

Tests that a checkbox input's bound signal is set to `true` when checked.

<div>
  <input id="clickable" type="checkbox" data-bind-result /> 
  <span data-text="$result"></span>
  <hr />
  Result:
  <code id="result" data-text="$result === true ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>