# Toggle All Path

Tests the toggle all action on a single path.

<div data-signals="{foo: false, result: 0}" data-on-load="@toggleAll('foo')">
  Result:
  <code id="result" data-text="$result = $foo ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>