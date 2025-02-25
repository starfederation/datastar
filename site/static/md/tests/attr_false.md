# Attr False

Tests that `data-attr-*` removes the element attribute when the value is `false`.

<div>
  <input type="text" data-ref-input data-attr-readonly="false" class="input input-bordered" />
  <hr />
  Result:
  <code id="result" data-text="$input.hasAttribute('readonly') ? 0 : 1"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>