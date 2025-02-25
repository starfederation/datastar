# Input Signal

Tests that a input's bound signal is not set to its value when a signal is defined.

<div data-signals-result="'foo'">
  <input type="text" data-bind-result value="bar" class="input input-bordered" />
  <hr />
  Result:
  <code id="result" data-text="$result === 'foo' ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>