# On Signal Change

Tests detecting a signal change.

<div data-signals="{foo: {bar: 0}, result: 0}" data-on-signal-change="$result = $foo.bar" data-on-load="$foo.bar = 1">
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>