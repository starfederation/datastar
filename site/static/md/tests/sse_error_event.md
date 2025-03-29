# SSE Error Event

Tests that an SSE error event is dispatched.

<div data-signals-result="0"
  data-on-datastar-sse="evt.detail.type == 'error' && $result++; events.innerHTML += evt.detail.type + '\n'" data-on-load="@get('/tests/sse_error_event/data')">
  <pre id="events"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>