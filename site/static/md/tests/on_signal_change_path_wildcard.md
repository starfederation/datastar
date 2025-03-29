# On Signal Change Path Wildcard

Tests detecting a signal change with a path using a wildcard.

<div data-signals="{foo: {bar: 0}, result: 0}" data-on-signal-change-foo.*="$result = $foo.bar" data-on-load="$foo.bar = 1">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>