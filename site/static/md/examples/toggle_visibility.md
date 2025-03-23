## Toggle Visibility

## Demo

<div id="container" data-init="@get('/examples/toggle_visibility/data')"></div>

## Explanation

This example displays how to toggle visibility using the `data-show` attribute.

```html
<div id="container" class="flex flex-col gap-4" data-signals="{bindBool: false}">
  <button class="btn btn-primary" data-on-click="$bindBool=!$bindBool">
    Toggle
  </button>
  <div data-show="$bindBool">
    Hello!
  </div>
</div>
```