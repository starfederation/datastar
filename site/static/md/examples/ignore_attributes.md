## Ignore Attributes

## Demo

<div data-signals-foo="'A valid signal'" class="text-primary">
  <div data-star-ignore__self data-text="$notASignal">
    <div data-text="$foo"></div>
  </div>
</div>

## Explanation

```html
<div data-signals-foo="'A valid signal'">
  <div data-star-ignore__self data-text="$notASignal">
    <div data-text="$foo"></div>
  </div>
</div>
```

This demonstrates how adding the `data-star-ignore__self` attribute to an element prevents the element from being processed by Datastar.

The `__self` modifier limits the behavior to the element itself. Removing the `__self` modifier would cause Datastar also ignore the descendant elements.

```html
<div data-star-ignore data-text="$notASignal">
    <div data-text="$alsoNotASignal"></div>
</div>
```