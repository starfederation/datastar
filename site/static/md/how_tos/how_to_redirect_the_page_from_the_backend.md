## How to redirect the page from the backend

## Intro

The [`datastar-execute-script`](/reference/sse_events#datastar-execute-script) SSE event allows the server to execute JavaScript on the browser.
While this might seem like a simple task, there are some browser-specific caveats to be aware of.

## Goal

Our goal is to show how to redirect to a different page from the backend using SSE.

## Steps

In this example we'll redirect the user to the `/foo` page:

```
event: datastar-execute-script
data: script setTimeout(() => window.location.href = "/foo")
```

Check out [Issue #529](https://github.com/starfederation/datastar/issues/529) to find out more why you need to execute this inside of `setTimeout`.

Another method for redirecting uses the [History API](https://developer.mozilla.org/en-US/docs/Web/API/History/pushState):
- This method is useful for changing the URL without reloading (e.g. building a search page that returns results as you type but also keeps your search query in the URL).
- You'll probably want to use this in combination with sending some signals/fragments, otherwise the site won't change.

```
event: datastar-execute-script
data: script window.history.pushState({}, "", "/search?query=dog")
```

Finally, you can also utilize `replaceState` in order to replace the current URL instead. This is useful when you don't want the previous URL to show up in your back/forward browser navigation:

```
event: datastar-execute-script
data: script window.history.replaceState({}, "", "/search?query=dog")
```

## Conclusion

We've covered three approaches for redirecting the page, allowing us to control what happens to the current history item.
