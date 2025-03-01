# Radio Value

Tests that a radio input's bound signal is assigned to the value when checked.

<div data-signals-result="'foo'">
  <input type="radio" data-bind-result value="foo" /> foo
  <br>
  <input id="clickable" type="radio" data-bind-result value="bar" /> bar
  <hr />
  Result:
  <code id="result" data-text="$result === 'bar' ? 1 : 0"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>