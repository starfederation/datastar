# Select Single

Tests that a single select element's bound signal is assigned to the selected option value.

<div data-signals-result="'foo'">
  <select id="selectable" data-bind-result class="select select-bordered"><option value="foo">foo</option><option value="bar">bar</option></select>
  <hr />
  Result:
  <code id="result" data-text="$result === 'bar' ? 1 : 0"></code>
  <hr />
  Expected result on selecting `bar`: <code>1</code>
</div>