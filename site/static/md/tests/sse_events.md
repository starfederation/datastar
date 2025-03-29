# SSE Events

Tests that SSE events are dispatched.

<div data-signals-result="0" data-on-datastar-sse="$result++; events.innerHTML += evt.detail.type + '\n'" data-on-load="@get('/tests/sse_events/data')">
  <pre id="events"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result == 2 ? 1 : 0"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>