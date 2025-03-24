# SSE Events

Tests that SSE events are dispatched on the element itself.

<div data-signals-result="0" data-on-load="@get('/tests/sse_events/data')"
  data-on-datastar-sse="$result++; events.innerHTML += evt.detail.type + '\n'">
  <pre id="events"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result == 2 ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>