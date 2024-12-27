## Custom Events

## Demo

<div data-signals="{eventCount:0,eventDetails:''}">
     <div id="foo" data-on-myevent="eventDetails=evt.detail;eventCount++">
          <div>Event count: <span id="eventCount" data-text="eventCount">EventCount</span></div>
          <div>Last Event Details: <span data-text="eventDetails">EventTime</span></div>
     </div>
     <script>
          const foo = document.getElementById('foo');
          setInterval(() => {
               foo.dispatchEvent(new CustomEvent('myevent', {
                    bubbles: true,
                    cancelable: true,
                    detail: JSON.stringify({ eventTime: new Date().toLocaleTimeString() })
               }));
          }, 1000);
     </script>
</div>

## Explanation

```html
<div data-signals="{eventCount:0,eventDetails:''}">
  <div id="foo" data-on-myevent="eventCount++; eventDetails=evt.detail">
    <div>Event count: <span data-text="eventCount">EventCount</span></div>
    <div>
      Last Event Details: <span data-text="eventDetails">EventTime</span>
    </div>
  </div>
  <script>
    const foo = document.getElementById("foo");
    setInterval(() => {
      foo.dispatchEvent(
        new CustomEvent("myevent", {
          bubbles: true,
          cancelable: true,
          detail: JSON.stringify({
            eventTime: new Date().toLocaleTimeString(),
          }),
        })
      );
    }, 1000);
  </script>
</div>
```

Datastar `data-on-*` plugin can listen to any event, including custom events. In this example, we are listening to a custom event `myevent` on the `foo` element. When the event is triggered, the event details are signalsd in the `eventDetails` signals and the `eventCount` signals is incremented. The event is triggered every second using `setInterval` in the script tag.

This is primarily used when interacting with Web Components or other custom elements that emit custom events.

**_Note:_** There is an extra variable `evt` available in the event handler that contains the event object. This can be used to access the event details like `evt.detail` in this example.
