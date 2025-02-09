## Signals If Missing on Load

## Demo

<div data-signals-id__ifmissing="1234">
  <div data-signals-id__ifmissing="5678">
    Should always be 1234:
    <span data-text="$id" class="text-primary font-bold"></span>
  </div>
</div>

## Explanation

```html
<div data-signals-id__ifmissing="1234">
  <div data-signals-id__ifmissing="5678">
    Should always be 1234:
    <span data-text="$id"></span>
  </div>
</div>
```