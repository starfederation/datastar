# Signals JSON

Tests that signals JSON function reacts to changes in signals.

<div data-signals="{foo: 0}">
  <pre id="output" data-text="ctx.signals.JSON()"></pre>
  <div data-on-load="$foo = 1"></div>
  <hr />
  Result:
  <code id="result" data-text="output.innerText.includes(': 1') ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>