## Active Search

[Original HTMX Version](https://htmx.org/examples/active-search/)

## Explanation

This example actively searches a contacts database as the user enters text.

## Demo

<div>
<div id="active_search" data-on-load="$$get('/examples/active_search/updates')"></div>
</div>

The interesting part is the input field:

```html
<input
  data-model="search"
  data-on-input.debounce_1000ms="$$get('/examples/active_search/data')"
  placeholder="Search..."
  type="text"
/>
```

The input issues a `GET` to `/active_search/data` with the input value bound to `$search`. The `debounce_1000ms` modifier ensures that the search is not issued on every keystroke, but only after the user has stopped typing for 1 second. This modifiers will be covered in more detail in the [reference section](/reference).
