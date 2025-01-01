## Dispatch Custom Event

## Demo

<div data-on-load="@get('/examples/dispatch_custom_event/events')">
    <div>Custom event "example-event-from-server" detail from server:</div>
    <pre class="mockup-code" id="container"></pre>
</div>


## Explanation

You can dispatch custom events from the server to the client. This is useful for when you need to trigger a client-side action from the server. The client can listen for the event and respond accordingly. This is useful for when you need to update the UI based on server-side events.

***Note:*** In general you want to use signals for reactivity.  This is a stop gap to interact with legace code or libraries that don't support signals.