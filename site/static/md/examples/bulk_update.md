## Bulk Update

[Original HTMX Version](https://htmx.org/examples/bulk-update/)

## Demo

<div id="bulk_update" data-init="@get('/examples/bulk_update/data')">
</div>

## Explanation

This demo shows how to implement a common pattern where rows are selected and then bulk updated. This is accomplished by
putting a form around a table, with checkboxes in the table, and then including the checked values in PUT’s to two
different endpoints: `activate` and `deactivate`.

The server will either activate or deactivate the checked users and then rerender the tbody tag with updated rows.