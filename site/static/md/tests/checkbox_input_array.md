# Checkbox Input Array

Tests that a checkbox input's value is added to a bound signal array when checked.

<div data-signals-result="['foo']">
  <input type="checkbox" data-bind-result value="foo" /> foo
  <br>
  <input id="clickable" type="checkbox" data-bind-result value="bar" /> bar
  <pre data-text="$result"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result.includes('foo') && $result.includes('bar') ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>
