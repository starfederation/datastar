## Cascading Selects

[Original HTMX Version](https://htmx.org/examples/value-select/)

## Demo

<div
    id="value_select"
    data-on-load="@get('/examples/value_select/data')"
>
</div>

## Explanation

In this example we show how to make the values in one select depend on the value selected in another select.

To begin we start with a default value for the make select: Audi. We render the model select for this make. We then have the make select trigger a GET to /models to retrieve the models options and target the models select.

Here is the code:

```html
<div id="value_select" data-signals='{"make":"","model":""}'>
  <div>Pick a Make / Model</div>
  <select data-bind="make" data-on-change="@get('/examples/value_select/data')">
    <option disabled>Select a Make</option>
    <option value="HYAABHANLTMQC">Audi</option>
    <option value="HZAABHANLTMQC">Toyota</option>
    <option value="H2AABHANLTMQC">Ford</option>
  </select>
</div>
```

When a request is made to the /models end point, we return the models for that make:

```html
<div id="value_select" data-signals='{"make":"HZAABHANLTMQC","model":""}'>
  <div>Pick a Make / Model</div>
  <select data-bind="make" data-on-change="@get('/examples/value_select/data')">
    <option disabled>Select a Make</option>
    <option value="HYAABHANLTMQC">Audi</option>
    <option value="HZAABHANLTMQC">Toyota</option>
    <option value="H2AABHANLTMQC">Ford</option>
  </select>
  <select
    data-bind="model"
    data-on-change="@get('/examples/value_select/data')"
  >
    <option disabled="" selected="" value="">Select a Model</option>
    <option value="HZIABHANLTMQC">Land Cruiser</option>
    <option value="HZQABHANLTMQC">Corolla</option>
    <option value="HZYABHANLTMQC">Camry</option>
  </select>
</div>
```

And they become available in the model select.
