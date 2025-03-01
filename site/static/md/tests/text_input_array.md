# Text Input Array

Tests that a text input's value is added to a bound signal array when changed.

<div data-signals-result="['foo']">
  <input class="input input-bordered" type="text" data-bind-result  /> foo
  <br>
  <input class="input input-bordered" type="text" data-bind-result id="populatable" /> bar
  <pre data-text="JSON.stringify($result)"></pre>
  <hr />
  Result:
  <code id="result" data-text="JSON.stringify($result) === JSON.stringify(['foo','bar']) ? 1 : 0"></code>
  <hr />
  Expected result after typing 'bar' in second input: <code>1</code>
</div>
