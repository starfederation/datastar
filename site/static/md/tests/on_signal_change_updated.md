# On Signal Change Updated

Tests detecting a signal change, and that only an updated signal is registered.

<div data-signals="{foo: 0, result: false}" data-on-signal-change="$result = evt.detail.added.length == 0 && evt.detail.updated.length == 1 && evt.detail.removed.length == 0" data-on-load="$foo = 1">
  Result:
  <code id="result" data-text="$result ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>