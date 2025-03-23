## Scroll into View

## Explanation

We are highlighting the middle paragraph of the page to make it easier to see the scroll into view effect. Depending on
the size of your browser window, you may need to scroll to see the effect. In this specific example the options are sent
to the server to then send down the scroll command, but normally you would just send the scroll command directly from
the server.

**Note:** The defaults are to have smooth centered scroll. If you want to also focus() the element just add the `focus`
modifier.

```html
<p data-scroll-into-view__instant__vstart__hcenter id="p10"></p>
```

## Demo

<div id="replaceMe" data-init="@get('/examples/scroll_into_view/data')">No session data</div>