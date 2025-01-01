## Bulk Update

[Original HTMX Version](https://htmx.org/examples/bulk-update/)

## Demo

<style>
tr.deactivate.datastar-settling td {
 background: lightcoral;
}
tr.activate.datastar-settling td {
 background: darkseagreen;
}
tr td {
transition: all 1.2s;
}
</style>

<div
    id="bulk_update"
    data-on-load="@get('/examples/bulk_update/data')"
>
</div>

## Explanation

This demo shows how to implement a common pattern where rows are selected and then bulk updated. This is accomplished by putting a form around a table, with checkboxes in the table, and then including the checked values in PUT’s to two different endpoints: `activate` and `deactivate`.

Added to the page in this way:

```html
<style>
  tr.deactivate.datastar-settling td {
    background: lightcoral;
  }
  tr.activate.datastar-settling td {
    background: darkseagreen;
  }
  tr td {
    transition: all 1.2s;
  }
</style>
```

The server will either activate or deactivate the checked users and then rerender the tbody tag with updated rows. It will apply the class activate or deactivate to rows that have been mutated. This allows us to use a bit of CSS to flash a color helping the user see what happened. The server events look like this:

```go
event: datastar-merge-fragments
id: 129798448043016254
data: selector
data: mergeMode upsertAttributes
data: settleDuration 500
data: fragments ...
```

Notice the `settle` value is 500. This means the `datastar-settling` class will be added immediately and removed after 500ms. This is a nice way to show the user that something is happening.
