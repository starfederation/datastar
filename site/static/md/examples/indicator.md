## Indicator

## Demo

<div class="flex flex-col gap-4">
  <div class="flex gap-2">
    <div class="loading-dots text-primary" data-class="{'loading ml-4': fetching}"></div>
    <button class="flex-1 btn btn-primary" data-on-click="sse('/examples/fetch_indicator/greet')" data-indicator="fetching"   data-attr-disabled="fetching" >
      Click me for a greeting
    </button>
  </div>
  <div id="greeting"></div>
</div>

## Explanation

```html
<style>
    .indicator {
        opacity:0;
        transition: opacity 300ms ease-out;
    }
    .indicator.loading {
        opacity:1;
        transition: opacity 300ms ease-in;
    }
</style>
<div
  class="indicator"
  data-class="{loading: fetching}"
>
    Loading Indicator
</div>
<button
  id="greetingBtn"
  data-on-click="sse('/examples/fetch_indicator/greet')"
  data-indicator-fetching
  data-attr-disabled="fetching"
>
  Click me for a greeting
</button>
<div id="greeting"></div>
```

The `data-indicator` attribute accepts the name of a signal whose value is set to `true` when a fetch request initiated from the same element is in progress, otherwise `false`. If the signal does not exist in the signals, it will be added.

***Note:*** If you use the `data-indicator` attribute, you ***MUST*** also make sure to have a unique `id` attribute on the element that is making the fetch request.  The is because the element might not exist otherwise nor be stable when the fetch request is completed.
