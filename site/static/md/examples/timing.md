## Timing

## Demo

<div 
  data-signals-count="0"
  data-on-interval__duration.5s.leading="@get('/examples/polling/interval')"
  class="text-primary">
  <div>Count increments every 5s: <span data-text="$count"></span></div>
  <button data-on-click="@get('/examples/polling/interval')" class="btn btn-primary">Manual override (should not disrupt the interval)</button>
</div>