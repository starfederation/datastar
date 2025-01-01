## Sortable

[Original HTMX Version](https://htmx.org/examples/sortable/)

## Demo

<div class="flex flex-col gap-8" data-signals="{orderInfo:''}">
  <div class="text-lg">Order Info: <span class="font-bold" data-text="$orderInfo">Order Info</span></div>
  <div>Open your console to see an event results</div>
  <div id="sortContainer" data-on-orderinfo="$orderInfo = event.detail.orderInfo; console.log(`You could send this to the server! ${$orderInfo}`)" class="flex flex-col gap-4">
    <div class="bg-primary text-primary-content p-4 rounded-box">Item 1</div>
    <div class="bg-primary text-primary-content p-4 rounded-box">Item 2</div>
    <div class="bg-primary text-primary-content p-4 rounded-box">Item 3</div>
    <div class="bg-primary text-primary-content p-4 rounded-box">Item 4</div>
    <div class="bg-primary text-primary-content p-4 rounded-box">Item 5</div>
  </div>
  <script type="module" src="/static/js/sortable.js"></script>
</div>

### Explanation

In the original example you had to hook into HTMX's events.  With Datastar, you can easily listen for custom events using `data-on-*`, and update signals to effect change.

```html
<div data-signals="{orderInfo:''}">
  <div>OrderInfo: <span data-text="$orderInfo">Order Info</span></div>
  <div id="sortContainer"
    data-on-reordered="$orderInfo = event.detail.orderInfo; console.log(`You could send this to the server! ${$orderInfo}`)"
  >
    <div>Item 1</div>
    <div>Item 2</div>
    <div>Item 3</div>
    <div>Item 4</div>
    <div>Item 5</div>
  </div>
  <script type="module" src="/static/js/sortable.js"></script>
</div>
```

The HTML looks pretty straightforward.  Create an `orderInfo` signal and modify it (and send to the server) whenever a `reordered` event is triggered.

```js
import Sortable from 'https://cdn.jsdelivr.net/npm/sortablejs@1.15.3/+esm'

const sortContainer = document.getElementById('sortContainer')

new Sortable(sortContainer, {
    animation: 150,
    ghostClass: 'opacity-25',
    onEnd: (evt) => {
        sortContainer.dispatchEvent(new CustomEvent('reordered', {detail: {orderInfo: `Moved from ${evt.oldIndex} to ${evt.newIndex}`}}));
    }
})
```

Here we are dispatching a custom event `reordered` whenever the sortable list is changed.  This event contains the order information that we can use to update the `orderInfo` signal.

Anything you can do in HTMX or Alpine should be possible in Datastar.
