## Click to Load

[Original HTMX Version](https://htmx.org/examples/click-to-load/)

## Demo

<div
    id="click_to_load"
    data-on-load="@sse('/examples/click_to_load/data')"
>
</div>

## Explanation

This example shows how to implement click-to-load the next page in a table of data. The crux of the demo is the final row:

```html
<button
  id="more_btn"
  data-on-click="$offset=30; $limit=10; @sse('/examples/click_to_load/data')"
>
  Load More
</button>
```

This button respond with a set of fragments in a text/event-stream with the next page of results (which will contain a button to load the next page of results). And so on.

```html
<!-- Removed styling for brevity -->
event: datastar-merge-fragments id: 129804115990544446 data: mergeMode morph data:
settleDuration 500 data: fragment
<button
  id="more_btn"
  data-on-click="$offset=20; $limit=10; @sse('/examples/click_to_load/data')"
>
  Load More
</button>
backend.ts:201 Received event block: event: datastar-merge-fragments id:
129804153571508286 data: selector #click_to_load_rows data: mergeMode append
data: settleDuration 500 data: fragment
<tr id="agent_10">
  <td>Agent Smith</td>
  <td>void11@null.org</td>
  <td class="uppercase">50698444ed39c832</td>
</tr>
backend.ts:201 Received event block: event: datastar-merge-fragments id:
129804153571512382 data: selector #click_to_load_rows data: mergeMode append
data: settleDuration 500 data: fragment
<tr id="agent_11">
  <td>Agent Smith</td>
  <td>void12@null.org</td>
  <td class="uppercase">205381dc855b977a</td>
</tr>
backend.ts:201 Received event block: id: 129804153571516478 data: selector
#click_to_load_rows data: mergeMode append data: settleDuration 500 data: fragment
<tr id="agent_12">
  <td>Agent Smith</td>
  <td>void13@null.org</td>
  <td class="uppercase">7ecd2e572c949f74</td>
</tr>
```
