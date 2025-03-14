# How to redirect the page from the backend

## Intro

Redirecting to another page is a common task that can be done from the backend using the [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE event.

## Goal

Our goal is to indicate to the user that they will be redirected, wait 3 seconds, and then redirect them to `/guide`, all from the backend.

## Demo

<button data-on-click="@get('/how_tos/redirect/data')" class="btn btn-primary font-bold">
Click to be redirected from the backend
</button>
<br/>
<span id="indicator" class="text-primary font-bold"></span>

## Steps

We'll place a `data-on-click` attribute on a button and use the `@get` action to send a `GET` request to the backend. We'll include an empty indicator `div` to show the user that they will be redirected.

```html
<button data-on-click="@get('/endpoint')">
    Click to be redirected from the backend
</button>
<div id="indicator"></div>
```

We'll set up our backend to first send a [`datastar-merge-fragments`](/reference/sse_events#datastar-merge-fragments) event with a populated indicator fragment, then wait 3 seconds, and finally send a [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE event to execute the JavaScript required to redirect the page.

```
event: datastar-execute-script
data: script window.location.href = "/guide"
```

Here's how it might look using the SDKs.

!!!CODE_SNIPPET:how_tos/redirect_1!!!

Note that in Firefox, if a redirect happens within a `script` tag then the URL is _replaced_, rather than _pushed_, meaning that the previous URL won't show up in the back history (or back/forward navigation).

To work around this, you can wrap the redirect in a `setTimeout` function call. See [issue #529](https://github.com/starfederation/datastar/issues/529) for reference.

!!!CODE_SNIPPET:how_tos/redirect_2!!!

Some SDKs provide a helper method that automatically wraps the statement in a `setTimeout` function call, so you don't have to worry about doing so (you're welcome!).

!!!CODE_SNIPPET:how_tos/redirect_3!!!

## Conclusion

Redirecting to another page can be done from the backend thanks to the ability to execute JavaScript on the frontend using the [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE event.
