## Click Outside

## Demo

<div data-signals-open="false" data-on-click__outside="$open ? $open = false : null" >
  <div data-show="$open" class="p-10 bg-green-700">
    <div class="p-10 bg-red-700">
      Click anywhere outside the green box to close
    </div>
  </div>
  <button data-on-click="$open = true" data-show="!$open" class="btn btn-primary">
    Open a box
  </button>
</div>

## Explanation

The `__outside` modifier is used to listen to clicks outside the element.

```html
<div data-signals-open="false" data-on-click__outside="$open ? $open = false : null" >
  <div data-show="$open" class="p-10 bg-green-700">
    <div class="p-10 bg-red-700">
      Click anywhere outside the green box to close
    </div>
  </div>
  <button data-on-click="$open = true" data-show="!$open">
    Open a box
  </button>
</div>
```