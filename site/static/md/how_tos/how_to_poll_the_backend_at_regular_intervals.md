# How to poll the backend at regular intervals

## Intro

Polling is a pull-based mechanism for fetching data from the server at regular intervals. It is useful when you want to
refresh the UI on the frontend, based on real-time data from the backend.

This in contrast to a push-based mechanism, in which a long-lived SSE connection is kept open between the client and the
server, and the server pushes updates to the client whenever necessary. Push-based mechanisms are more efficient than
polling, and can be achieved using Datastar, but may be less desirable for some backends.

In PHP, for example, keeping long-lived SSE connections is fine for a dashboard in which users are authenticated, as the
number of connections are limited. For a public-facing website, however, it is not recommended to open many long-lived
connections, due to the architecture of most PHP servers.

## Goal

Our goal is to poll the backend at regular intervals (starting at 5 second intervals) and update the UI accordingly. The
backend will determine changes to the DOM and be able to control the rate at which the frontend polls based on some
criteria. For this example, we will simply output the server time, increasing the polling frequency to 1 second during
the last 10 seconds of every minute. The criteria could of course be anything such as the number of times previously
polled, the user's role, load on the server, etc.

## Demo

<div id="time" data-on-interval__duration.5s.leading="@get('/how_tos/polling/data')" class="text-primary font-bold">
</div>

## Steps

The `data-on-interval` attribute allows us to execute an expression at a regular interval. We'll use it to send a `GET`
request to the backend, and use the `__duration` modifier to set the interval duration.

```html
<div id="time" data-on-interval__duration.5s="@get('/endpoint')"></div>
```

In addition to the interval, we could also execute the expression immediately by adding `.leading` to the modifier.

```html
<div id="time" data-on-interval__duration.5s.leading="@get('/endpoint')"></div>
```

Most of the time, however, we'd just render the current time on page load using a backend templating language.

```html
<div id="time" data-on-interval__duration.5s="@get('/endpoint')">
     {{ now }}
</div>
```

Now our backend can respond to each request with a
[`datastar-merge-fragments`](/reference/sse_events#datastar-merge-fragments) event with an updated version of the
element.

```
event: datastar-merge-fragments
data: fragments <div id="time" data-on-interval__duration.5s="@get('/endpoint')">
     data: fragments {{ now }}
     data: fragments </div>
```

Be careful not to add `.leading` to the modifier in the response, as it will cause the frontend to immediately send
another request.

Here's how it might look using the SDKs.

!!!CODE_SNIPPET:how_tos/polling_1!!!

Our second requirement was that the polling frequency should increase to 1 second during the last 10 seconds of every
minute. To make this possible, we'll calculate and output the interval duration based on the current seconds of the
minute.

!!!CODE_SNIPPET:how_tos/polling_2!!!

## Conclusion

Using this approach, we not only end up with a way to poll the backend at regular intervals, but we can also control the
rate at which the frontend polls based on whatever criteria our backend requires.