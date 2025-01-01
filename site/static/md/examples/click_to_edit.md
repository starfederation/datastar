## Click to Edit

[Original HTMX Version](https://htmx.org/examples/click-to-edit/)

## Demo

<div
    id="contact_1"
    data-on-load="@sse('/examples/click_to_edit/contact/1')"
>

</div>

## Explanation

The click to edit pattern provides a way to offer inline editing of all or part of a record without a page refresh. This pattern starts with a UI that shows the details of a contact. The div has a button that will get the editing UI for the contact from `/contact/1/edit`

```html
<!-- Removed styling -->
<div id="contact_1">
  <label>First Name: John</label>
  <label>Last Name: Doe</label>
  <label>Email: joe@blow.com</label>
  <div>
    <button data-on-click="@sse('/examples/click_to_edit/contact/1/edit')">
      Edit
    </button>
    <button data-on-click="@sse('/examples/click_to_edit/contact/1/reset')">
      Reset
    </button>
  </div>
</div>
```

This returns a form that can be used to edit the contact

```html
<!-- Removed styling and escaping for brevity -->
<div
  id="contact_1"
  data-signals="{
        id: 1,
        firstName: 'John',
        lastName: 'Doe',
        email: 'joe@blow.com'
    }"
>
  <div class="form-control">
    <label>First Name</label>
    <input type="text" data-bind="firstName" />
  </div>
  <div>
    <label>Last Name</label>
    <input type="text" data-bind="lastName" />
  </div>
  <div>
    <label>Email</label>
    <input type="text" data-bind="email" />
  </div>
  <div>
    <button data-on-click="@sse('/examples/click_to_edit/contact/1',{method:'put'})">
      Save
    </button>
    <button data-on-click="@sse('/examples/click_to_edit/contact/1')">
      Cancel
    </button>
  </div>
</div>
```

### There is no form

If you compare to HTMX you'll notice there is no form, you can use one, but it's unnecessary. This is because you are already using signals and when you use a `PUT` to `/contact/1/edit`, the body is the entire contents of the signals, and it's available to handle errors and validation holistically. There is also a profanity filter on the normal rendering of the contact that is not applied to the edit form. Controlling the rendering complete on the server allows you to have a single source of truth for the data and the rendering.

### There is no client validation

On the backend we've also added a quick sanitizer on the input to avoid bad actors (to some degree). You already have to deal with the data on the server so you might as well do the validation there. In this case its just modifying how the text is rendered when not editing. This is a simple example, but you can see how you can extend it to more complex forms.
