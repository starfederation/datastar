## Optimistic Update

While optimistic updates are not ideal, they are achievable with DataStar. The big drawback with this style of UX is 
that the users will see the update happening before it actually happens. Care must be taken to indicate to the user that
this update is not fully persisted and may still fail.

See the [indicator example](/examples/indicator) for a more correct way to indicate the state of a fetch request.

## Demo


<div>
  <div data-signals="{fetching: false, name:''}">
    <div  class="text-primary">
        <input id="name" data-attr-disabled="$fetching" type="text" data-bind="name">
        <button class="flex-1 btn btn-primary" data-indicator-fetching data-on-click="@post('/examples/optimistic_update/update_data')" data-attr-disabled="$fetching" >
            Save
        </button>
    </div>
    <div id="list">
        <div id="optimistic" class="text-primary" data-show="$fetching" data-text="$name"></div>
    </div>
  </div>
</div>

## Explanation

```html

<div>
    <div data-signals="{fetching: false, name:''}">
        <div  class="text-primary">
            <input  id="name" data-attr-disabled="$fetching" type="text" data-bind="name">
            <button class="flex-1 btn btn-primary" data-indicator-fetching data-on-click="@post('/examples/optimistic_update/update_data')" data-attr-disabled="$fetching" >
                Save
            </button>
        </div>
        <div id="list">
            <div id="optimistic" class="text-primary" data-show="$fetching" data-text="$name"></div>
        </div>
    </div>
</div>
```

The `data-indicator` attribute accepts the name of a signal whose value is set to `true` when a fetch request initiated
from the same element is in progress, otherwise `false`. If the signal does not exist in the signals, it will be added.

Using that we can selectively show the result of the update. Here it remains yellow until the fetch request is complete.
When complete the backend sends down the fragment to be prepended to the list and resets the `$name` signal. Note that 
for the sake of demonstration the backend is artificially slow and takes 2 seconds to respond. 

