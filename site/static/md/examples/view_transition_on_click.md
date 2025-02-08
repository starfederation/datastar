## View Transition On Click

## Demo

<div class="flex flex-col gap-4" data-signals="{show: true}">
  <div data-text="`View Transition API supported in browser? ` + !!document.startViewTransition"></div>
  <div class="flex gap-4" data-show="$show">
    <button class="btn btn-accent" data-on-click__viewtransition="$show = false">
      Fade out
    </button>
  </div>
</div>

## Explanation

```html
<div data-signals="{show: true}">
  <div data-text="`View Transition API supported in browser? ` + !!document.startViewTransition"></div>
  <div data-show="$show">
    <button class="btn btn-accent" data-on-click__viewtransition="$show = false">
      Fade out
    </button>
  </div>
</div>
```