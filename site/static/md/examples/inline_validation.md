## Inline Validation

[Original HTMX Version](https://htmx.org/examples/inline-validation/)

## Demo

The only email that will be accepted is test@test.com.

<div
    id="inline_validation"
    data-on-load="@get('/examples/inline_validation/data')"
>
</div>

## Explanation

This example shows how to do inline field validation, in this case of an email address. To do this we need to create a form with an input that POSTs back to the server with the value to be validated and updates the DOM with the validation results. Since its easy to replace the whole form the logic for displaying the validation results is kept simple.
