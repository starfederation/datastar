# Set All Path

Tests the set all action on multiple paths.

<div data-signals="{foo: false, bar: false, result: 0}" data-on-load="@setAll('foo bar', true)">
  Result:
  <code id="result" data-text="$result = $foo && $bar ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>