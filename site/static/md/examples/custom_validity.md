## Custom Validity

## Demo

<form data-on-submit="sse('/examples/custom_validity/data', {contentType: 'form'})" class="space-y-8">
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-foo data-custom-validity="foo.value === bar.value ? '' : 'Not da same'" name="foo" class="grow" placeholder="Type foo contents"/>
  </label>
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-bar name="bar" class="grow" placeholder="Type bar contents"/>
  </label>
  <div class="space-x-4">
    <button class="btn btn-primary">
      Submit form
    </button>
  </div>
</form>

## Explanation

```html
<form data-on-submit="sse('/endpoint', {contentType: 'form'})">
  <input data-bind-foo data-custom-validity="foo.value === bar.value ? '' : 'Not da same'" name="foo" placeholder="Type foo contents">
  <input data-bind-bar name="bar" placeholder="Type bar contents">
  <button>
      Submit form
  </button>
</form>
```

## Flawed Demo

<form data-on-submit="sse('/examples/custom_validity/data', {contentType: 'form'})" class="space-y-8">
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-fizz data-on-input="ctx.el.setCustomValidity(fizz.value === buzz.value ? '' : 'Not da same')" name="fizz" class="grow" placeholder="Type fizz contents"/>
  </label>
  <label class="flex items-center gap-2 input input-bordered">
    <input data-bind-buzz name="buzz" class="grow" placeholder="Type buzz contents"/>
  </label>
  <div class="space-x-4">
    <button class="btn btn-primary">
      Submit form
    </button>
  </div>
</form>

## Explanation

```html
<form data-on-submit="sse('/endpoint', {contentType: 'form'})">
  <input data-bind-fizz data-on-input="ctx.el.setCustomValidity(fizz.value === buzz.value ? '' : 'Not da same')" name="fizz" placeholder="Type fizz contents">
  <input data-bind-buzz name="buzz" placeholder="Type buzz contents">
  <button>
      Submit form
  </button>
</form>
```