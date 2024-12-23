## Disabled Button

## Demo

<style>
    #target {
      padding: 10px;
      background-color: #4CAF50;
      color: white;
      border: none;
      cursor: pointer;
      border-radius: 5px;
    }
    #target[disabled] {
      opacity: 0.25;
      cursor: not-allowed;
    }
</style>

<div id="container" data-signals="{shouldDisable:false}">
  <button
    id="target"
    data-on-click="shouldDisable.value=true ; sse('/examples/disable_button/data')"
    data-attr-disabled="shouldDisable.value"
  >Click Me</button>
</div>

## Explanation

```html
<div id="container" data-signals="{shouldDisable:false}">
  <button
    id="target"
    data-on-click="shouldDisable.value = true;sse('/examples/disable_button/data')"
    data-attr-disabled="shouldDisable.value"
  >
    Click Me
  </button>
  <div id="results">
    <h1>Results from server</h1>
  </div>
</div>
```

The example has been slowed down to show the disabled state of the button. By using signals you can disable the button and then re-enable it with a server response. In general signals allow for general reactivity that would otherwise have to be extra code paths such as [hx-disable-elt](https://htmx.org/attributes/hx-disable-elt/) in HTMX.
