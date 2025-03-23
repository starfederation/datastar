## Delete Row

[Original HTMX Version](https://htmx.org/examples/delete-row/)

## Demo

<div id="delete_row" data-init="@get('/examples/delete_row/data')">
</div>

## Explanation

This example shows how to implement a delete button that removes a table row upon completion. First let's look at the
table body:

```html
<table class="table">
  <thead>
    <tr>
      <th>Name</th>
      <th>Email</th>
      <th>Status</th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr id="contact_0">
      <td>Joe Smith</td>
      <td>joe@smith.org</td>
      <td>Active</td>
      <td>
        <button data-on-click="confirm('Are you sure?') && @delete('/examples/delete_row/data/0')">
          Delete
        </button>
      </td>
    </tr>
    ...
  </tbody>
</table>
```

The row has a normal [`confirm`](https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm) to confirm the delete
action.