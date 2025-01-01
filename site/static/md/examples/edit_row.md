## Edit Row

[Original HTMX Version](https://htmx.org/examples/edit-row/)

## Demo

<div
    id="edit_row"
    data-on-load="@get('/examples/edit_row/data')"
>
</div>

## Explanation

This example shows how to implement editable rows. First let’s look at the row prior to editing:

```html
<tr id="contact_0">
  <td>Joe Smith<//td>
  <td>joe@smith.org</td>
  <td>
    <button data-on-click="$editRowIndex=0;@get('/examples/edit_row/edit')" >Edit</button>
  </td>
</tr>
```

This will tell trigger a whole table replacement as we are going to remove the `Edit` buttons from other rows as well as change out to inputs to allow editing. The `data-on-click` attribute sets a variable `$editRowIndex` to the index of the row that is being edited. This is used to determine which row to edit on the server side. Again we don't need alpine or hyperscript to do this, signals and actions are built-in.

Finally, here is what the row looks like when the data is being edited:

```html
<tr id="contact_0">
  <td>
    <input data-bind-name type="text" />
  </td>
  <td>
    <input data-bind-email type="text" />
  </td>
  <td>
    <button data-on-click="@get('/examples/edit_row/data')">Cancel</button>
    <button data-on-click="@patch('/examples/edit_row/edit')">Save</button>
  </td>
</tr>
```

Here we have a few things going on, clicking `Cancel` will bring back the read-only version of the row. Finally, there is a save button that issues a `PATCH` to update the contact.
