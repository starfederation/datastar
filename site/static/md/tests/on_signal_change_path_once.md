# On Signal Change Path Once

Tests detecting a signal change with a path, and that the expression is called once.

<div data-signals="{foo: {bar: 0}, result: 0}" data-on-signal-change-foo.bar="$result++" data-on-load="$foo.bar = 1">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>