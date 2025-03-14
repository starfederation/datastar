# Input Array

Tests that a text input's value is added to a bound signal array when changed.

<div data-signals-result="['foo']">
  <input data-bind-result class="input input-bordered" />
  <br>
  <input id="populatable" data-bind-result class="input input-bordered" />
  <pre data-text="JSON.stringify($result)"></pre>
  <hr />
  Result:
  <code id="result" data-text="JSON.stringify($result) === JSON.stringify(['foo','bar']) ? 1 : 0"></code>
  <hr />
  Expected result on entering `bar` in second input: <code>1</code>
</div>
