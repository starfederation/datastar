## Form Data

## Demo

<form id="myform" class="space-y-8">
  <label class="flex items-center gap-2 input input-bordered">
    <input name="foo" required class="grow" placeholder="Type foo contents"/>
  </label>
  <div class="space-x-4">
    <button data-on-click="sse('/examples/form_data/data', {contentType: 'form'})" class="btn btn-primary">
      Submit GET request
    </button>
    <button data-on-click="sse('/examples/form_data/data', {contentType: 'form', method: 'post'})" class="btn btn-primary">
      Submit POST request
    </button>
  </div>
</form>

<button data-on-click="sse('/examples/form_data/data', {contentType: 'form', selector: '#myform'})" class="btn btn-primary">
  Submit GET request from outside the form
</button>

## Explanation

Setting the `contentType` option to `form` tells the `sse()` action to look for the closest form, perform validation on it, and send all form elements within it to the backend. A `selector` option can be provided to specify a form element. No signals are sent to the backend in this type of request.

```html
<form>
  <input name="foo" required placeholder="Type foo contents">
  <button data-on-click="sse('/endpoint', {contentType: 'form'})">
    Submit GET request
  </button>
  <button data-on-click="sse('/endpoint', {contentType: 'form', method: 'post'})">
    Submit POST request
  </button>
</form>

<button data-on-click="sse('/endpoint', {contentType: 'form', selector: '#myform'})">
  Submit GET request from outside the form
</button>
```

## Demo

<form data-on-submit="sse('/examples/form_data/data', {contentType: 'form'})" class="space-y-8">
  <label class="flex items-center gap-2 input input-bordered">
    <input name="bar" required class="grow" placeholder="Type bar contents"/>
  </label>
  <div class="space-x-4">
    <button class="btn btn-primary">
      Submit form
    </button>
  </div>
</form>

## Explanation

In this example, the `sse()` action is placed on the form itself using `data-on-submit`.

```html
<form data-on-submit="sse('/endpoint', {contentType: 'form'})">
  <input name="bar" placeholder="Type bar contents" required>
  <button>
      Submit form
  </button>
</form>
```