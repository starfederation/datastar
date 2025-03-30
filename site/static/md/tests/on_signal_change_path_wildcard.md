# On Signal Change Path Wildcard

Tests detecting a signal change with a path using a wildcard.

<div data-signals="{foo: {bar: {baz: false}}, result: 0}" data-on-signal-change-foo.*.baz="$result = $foo.bar.baz ? 1 : 0" data-on-load="$foo.bar.baz = true">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>