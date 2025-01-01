## Replace URL from Backend

## Demo

<div
    data-on-load="sse('/examples/replace_url_from_backend/updates')"
></div>

## Explanation

Interacting with the history API is a common task when building single page applications. The [`datastar-execute-script`](/reference/attribute_plugins#data-datastar-execute-script) SSE event can be used to execute JavaScript on the client. This can be used to replace the URL in the browser without reloading the page.

```html
event: datastar-execute-script
retry: 1000
data: script window.history.replaceState({}, "", "/examples/replace_url_from_backend/updates?page=89")


event: datastar-execute-script
retry: 1000
data: script window.history.replaceState({}, "", "/examples/replace_url_from_backend/updates?page=39")


event: datastar-execute-script
retry: 1000
data: script window.history.replaceState({}, "", "/examples/replace_url_from_backend/updates?page=7")
```


Look at the browser URL and you should see it change periodically to a random page number.
