## Infinite Scroll

[Original HTMX Version](https://htmx.org/examples/infinite-scroll/)

## Explanation

The infinite scroll pattern provides a way to load content dynamically on user scrolling action.

Let’s focus on the final row (or the last element of your content):

```html
<div
  data-intersects="$offset=0;$limit=10;sse('/examples/infinite_scroll/data')"
  id="more_btn"
>
  <div>Loading...</div>
</div>
```

This last element contains a listener which, when scrolled into view, will trigger a request. The result is then appended after it. The last element of the results will itself contain the listener to load the next page of results, and so on. `data-intersects` is a custom attribute that triggers a request when the element is scrolled into view. The `offset` and `limit` parameters are used to control the pagination of the results. The server sends back a new `more_btn` element with a new offset and the results to be appended.

```bash
event: datastar-merge-fragments
data: selector #more_btn
data: mergeMode morph
data: settleDuration 500
data: fragments <div data-intersects="$offset=10;$limit=10;sse('/examples/infinite_scroll/data')" id="more_btn"><div>Loading...</div></div>

event: datastar-merge-fragments
data: selector #click_to_load_rows
data: mergeMode append
data: settleDuration 500
data: fragments <tr id=\"agent_60\"><td>Agent Smith 3c</td><td>void61@null.org</td><td class=\"uppercase\">39b02fcf39c047c5</td></tr>"

....More rows to be appended
```

## Demo

Don't scroll too far, there are consequences

<div>
<div
    id="infinite_scroll"
    data-on-load="sse('/examples/infinite_scroll/data')"
>
</div>
<div id="more_btn"></div>
</div>
