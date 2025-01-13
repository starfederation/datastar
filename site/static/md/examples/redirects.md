## Redirects

## Demo

<div id="update" data-on-load="@get('/examples/redirects/data')">
</div>

## Explanation

As part of SSE updates you may want to redirect the user to a different page. The [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE event can be used to execute JavaScript on the client.

```html
event: datastar-execute-script
data: script window.location = "/essays/grugs_around_fire"
```
