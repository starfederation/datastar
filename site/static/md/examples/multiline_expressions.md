# Multiline Expressions

## Demo

<div
	data-signals="{duration:0, lastRenderTime:Date.now()}"
  data-on-raf="
    const now = Date.now()
	const delta = now - $lastRenderTime
	$duration += delta / 1000
	$lastRenderTime = now
  "
>
	<div data-text="`Demo started ${Math.round($duration)} seconds ago`"></div>
</div>

## Explanation

```html
<div
  data-signals="{duration:0, lastRenderTime:Date.now()}"
  data-on-raf="
    const now = Date.now()
    const delta = now - $lastRenderTime
    $duration += delta / 1000
    $lastRenderTime = now
  "
>
  <div data-text="`Demo started ${Math.round($duration)} seconds ago`"></div>
</div>
```

Simple example showing the use of a multiline expression in a `data-*` attribute expression. While you want to long expressions, they can aid in [locality of behavior](https://htmx.org/essays/locality-of-behaviour/).

**Note:** The `data-on-raf` attribute updating every frame but if you look at the DOM in devtools you'll see it only updates the text every second. Signals allow for optimized updates as they only update the DOM when the value changes.
