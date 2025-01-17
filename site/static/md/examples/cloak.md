## Cloak

## Demo

<div>
  <style>
    .cloak {
        opacity: 0;
        transition: opacity 2s ease-in;
    }
  </style>
  <div class="cloak" id="cloak" data-class="{cloak:false}">
    Cloaked text
  </div>
</div>

## Explanation

This example displays how to prevent flickering of the element before Datastar has processed the DOM.
By removing the `cloak` class on initialisation, there will never be any flickering on load.

```html
<style>
  .cloak {
      opacity: 0;
      transition: opacity 2s ease-in;
  }
</style>

<div
  class="cloak"
  data-class="{cloak:false}"
>
  Cloaked text
</div>
```
