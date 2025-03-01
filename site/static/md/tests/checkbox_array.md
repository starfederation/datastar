# Checkbox Array

Tests that a checkbox input's value is added to a bound signal array when checked.

<div data-signals-result="['foo']">
  <input type="checkbox" data-bind-result value="foo" /> foo
  <br>
  <input type="checkbox" data-bind-result value="bar" /> bar
  <br>
  <input id="clickable" type="checkbox" data-bind-result value="baz" /> baz
  <pre data-text="JSON.stringify($result)"></pre>
  <hr />
  Result:
  <code id="result" data-text="JSON.stringify($result) === JSON.stringify(['foo','','baz']) ? 1 : (JSON.stringify($result) === JSON.stringify(['foo']) ? 0 : -1)"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>