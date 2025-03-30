# Set All Path Wildcard

Tests the set all action on a path using a wildcard.

<div data-signals="{foo: {bar: {baz: false}}, result: 0}" data-on-load="@setAll('foo.*.baz', true)">
  Result:
  <code id="result" data-text="$result = $foo.bar.baz ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>