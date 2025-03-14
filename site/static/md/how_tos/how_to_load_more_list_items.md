# How to load more list items

## Intro

Loading more list items into the DOM from the backend is a common alternative to pagination. What makes it different is that we need to append the new items to the existing list, rather than replace them.

## Goal

Our goal is to incrementally append list items into a specific part of the DOM, each time a button is clicked. Once five items are visible, the button should be removed.

## Demo

<div id="list" data-signals-offset="1">
    <div class="text-primary font-bold">Item 1</div>
</div>
<div>
    <button id="load-more" data-on-click="@get('/how_tos/load_more/data')" class="btn btn-primary font-bold">
        Click to load another item
    </button> 
</div>

## Steps

We'll give the list item container and the button unique IDs, so that we can target them individually.

We'll use a `data-signals-*` attribute to set the initial `offset` to `1`, and a `data-on-click` button that will send a `GET` request to the backend.

```html
<div id="list">
    <div>Item 1</div>
</div>
<button id="load-more" 
        data-signals-offset="1" 
        data-on-click="@get('/how_tos/load_more/data')">
    Click to load another item
</button> 
```

The backend will receive the `offset` signal and, if not above the max number of allowed items, will return the next item to be appended to the list.

We'll set up our backend to send a [`datastar-merge-fragments`](/reference/sse_events#datastar-merge-fragments) event with the `selector` option set to `#list` and the `mergeMode` option set to `append`. This tells Datastar to _append_ the fragments _into_ the `#list` container (rather than the default behaviour of replacing it).

```
event: datastar-merge-fragments
data: selector #list
data: mergeMode append
data: fragments <div>Item 2</div>
```

In addition, we'll send a [`datastar-merge-signals`](/reference/sse_events#datastar-merge-signals) event to update the `offset`.

```
event: datastar-merge-signals
data: signals {offset: 2)
```

In the case when all five list items have been shown, we'll remove the button from the DOM entirely.

```
event: datastar-remove-fragments
data: selector #load-more
```

Here's how it might look using the SDKs.

!!!CODE_SNIPPET:how_tos/load_more!!!

## Conclusion

While using the default merge mode of `morph` is generally recommended, appending to a list is a good example of when to use an alternative merge mode.