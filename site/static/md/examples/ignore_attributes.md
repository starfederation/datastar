## Ignore Attributes

## Demo

<div data-star-ignore data-text="$notASignal">
  <div data-text="$alsoNotASignal"></div>
</div>

## Explanation

```html
<div data-star-ignore data-text="$notASignal">
    <div data-text="$alsoNotASignal"></div>
</div>
```

This demonstrates how adding the `data-star-ignore` attribute to an element will prevent the element from and its descendants being processed by Datastar.

It is also possible to ignore an element but not its descendants by using the `__self` modifier. The following example will ignore the outer element but process the inner element.

```html
<div data-star-ignore__self data-text="$notASignal">
    <div data-text="$aValidSignal"></div>
</div>
```