# SSE Error Event

Tests that an SSE error event is dispatched.

<div data-signals-result="0" data-on-load="@get('/tests/sse_error_event/data')"
  data-on-datastar-sse="events.innerHTML += evt.detail.type + '\n'; evt.detail.type == 'error' && $result++">
  <pre id="events"></pre>
  <hr />
  Result:
  <code id="result" data-text="$result"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>