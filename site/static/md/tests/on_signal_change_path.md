# On Signal Change Path

Tests detecting a signal change with a path.

<div data-signals="{foo: 0, result: 0}" data-on-signal-change-foo="$result = $foo" data-on-load="$foo = 1">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>