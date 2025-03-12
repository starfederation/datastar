# On Resize

Tests that the on resize event is triggered when an element's dimensions are changed.

<div data-signals-result="0" data-on-load="$result = 0" data-on-load__delay.100ms="testEl.style = 'width: 10px'">
  <div id="testEl" data-on-resize="$result = 1"></div>
  Result:
  <code id="result" data-text="$result">0</code>
  <hr />
  Expected result on wait: <code>1</code>
</div>