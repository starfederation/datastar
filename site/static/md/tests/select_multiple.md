# Select Multiple

Tests that a multiple select element's bound signal is assigned to the selected option values.

<div data-signals-result="['foo']">
  <select id="selectable" data-bind-result multiple class="select select-bordered"><option value="foo">foo</option><option value="bar">bar</option></select>
  <pre data-text="JSON.stringify($result)"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result.includes('foo') && $result.includes('bar') ? 1 : 0"></code>
  <hr />
  Expected result on selecting `foo` and `bar`: <code>1</code>
</div>