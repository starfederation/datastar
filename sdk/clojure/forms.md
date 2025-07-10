# Divergence of behavior from beta11 to RC12 when it comes to forms

## The setup
To test the forms behavior I made little page that lets me send the content of a
text input to the server via a click on a button.

They are 3 parameter used in this test:
- the form method (GET or POST)
- whether the button has a type attribute of button or no such attribute
- whether the `contentType` option is used in the Datastar action

This give me a HTML page like this:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.11/bundles/datastar.js" ></script>
  </head>
  <body>
    <div>
      <form action="">
        <label for="input1">Enter text</label>
        <br />
        <input id="input1" type="text" name="input-1" data-bind-input-1 />
        <br />
        <br />

        <!-- Section where the different button behaviors are tested -->
        <button               data-on-click="@get('/endpoint')">                         get - nct - nt</button> <br />
        <button type="button" data-on-click="@get('/endpoint')">                         get - nct - t </button> <br />
        <button               data-on-click="@get('/endpoint', {contentType: 'form'})">  get - ct - nt </button> <br />
        <button type="button" data-on-click="@get('/endpoint', {contentType: 'form'})" > get - ct - t </button> <br />

        <br />

        <button               data-on-click="@post('/endpoint')">                         post - nct - nt</button> <br />
        <button type="button" data-on-click="@post('/endpoint')">                         post - nct - t </button> <br />
        <button               data-on-click="@post('/endpoint', {contentType: 'form'})">  post - ct - nt </button> <br />
        <button type="button" data-on-click="@post('/endpoint', {contentType: 'form'})" > post - ct - t </button>
        <br />
        <!-- END -->

      </form>
      <span id="form-result"></span>
    </div>
  </body>
</html>
```
The idea is that when I click one button I observe if I correctly get the value
from the input and where I get the value from.

> [!note]
> Note that the case where I don't set the type attribute on the button while
> not using the `contentType` option (regardless of the HTTP Method) is not a
> valid use of D* with forms.


| method | btn type="button" | contentType set | beta11                                      | RC12                     |
| ------ | ----------------- | --------------- | ------------------------------------------- | ------------------------ |
| GET    | no                | yes             | btn works as submit, expected HTML behavior | idem                     |
| GET    | no                | no              | values via signals in the query str         | idem                     |
| GET    | yes               | yes             | values the query str (HTML behavior)        | no value in query string |
| GET    | yes               | no              | values the query str (HTML behavior)        | no value in query string |
| POST   | no                | yes             | btn works as submit, expected HTML behavior | idem                     |
| POST   | no                | no              | values via signals in the request's body    | idem                     |
| POST   | yes               | yes             | values via the request body (HTML behavior) | idem                     |
| POST   | yes               | no              | values via the request body (HTML behavior) | idem                     |



