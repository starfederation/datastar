# JSON Merged Signals

Tests that the JSON attribute reacts to newly merged signals.

<div data-signals="{result: false}">
  <pre id="output" data-json></pre>
  <div data-on-load="ctx.signals.upsertIfMissing('foo', 1); $result = output.innerText.includes('foo')"></div>
  <hr />
  Result:
  <code id="result" data-text="$result ? 1 : 0">0</code>
  <hr />
  Expected result on load: <code>1</code>
</div>