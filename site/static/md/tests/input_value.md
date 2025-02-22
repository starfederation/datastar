# Input Value

Tests that a input's bound signal is set to its value when non-empty and no signal is defined.

<div>
  <input type="text" data-bind-result value="foo" class="input input-bordered" />
  <hr />
  Result:
  <code id="result" data-text="$result === 'foo' ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>