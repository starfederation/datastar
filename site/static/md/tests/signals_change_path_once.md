# Signals Change Path Once

Tests that a signal change with a path is detected and the expression is called once.

<div data-signals="{foo: {bar: 0}, result: 0}" data-on-signals-change-foo="$result++">
  <button id="clickable" data-on-click="$foo.bar = 1" class="btn">Change</button>
  <hr />
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>