# Getting Started

Datastar brings the functionality provided by libraries like [Alpine.js](https://alpinejs.dev/) (frontend reactivity) and [htmx](https://htmx.org/) (backend reactivity) together, into one cohesive solution. It's a lightweight, extensible framework that allows you to:

1. Manage state and build reactivity into your frontend using HTML attributes.
2. Modify the DOM and state by sending events from your backend.

With Datastar, you can build any UI that a full-stack framework like React, Vue.js or Svelte can, but with a much simpler, hypermedia-driven approach.

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        We're so confident that Datastar can be used as a JavaScript framework replacement that we challenge anyone to find a use-case for a web app that Datastar <em>cannot</em> realistically be used to build!
    </div>
</div>

## Installation

The quickest way to use Datastar is to include it in your HTML using a script tag hosted on a CDN.

```html
<script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.10/bundles/datastar.js"></script>
```

If you prefer to host the file yourself, download your own bundle using the [bundler](/bundler), then include it from the appropriate path.

```html
<script type="module" src="/path/to/datastar.js"></script>
```

You can alternatively install Datastar via [npm](https://www.npmjs.com/package/@starfederation/datastar). We don't recommend this for most use-cases, as it requires a build step, but it can be useful for legacy frontend projects.

```bash
npm install @starfederation/datastar
```

## Data Attributes

At the core of Datastar are [`data-*`](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes) attributes (hence the name). They allow you to add reactivity to your frontend in a declarative way, and to interact with your backend.

Datastar uses signals to manage state. You can think of signals as reactive variables that automatically track and propagate changes in expressions. They can be created and modified using data attributes on the frontend, or events sent from the backend. Don't worry if this sounds complicated; it will become clearer as we look at some examples.

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        The Datastar <a href="https://marketplace.visualstudio.com/items?itemName=starfederation.datastar-vscode">VSCode extension</a> and <a href="https://plugins.jetbrains.com/plugin/26072-datastar-support">IntelliJ plugin</a> provided autocompletion for all <code>data-*</code> attributes.
    </div>
</div>

### `data-bind`

Datastar provides us with a way to set up two-way data binding on an element using the [`data-bind`](/reference/attribute_plugins#data-bind) attribute, which can be placed on any HTML element on which data can be input or choices selected from (`input`, `textarea`, `select`, `checkbox` and `radio` elements, as well as web components).

```html
<input data-bind-input />
```

This creates a new signal that can be called using `$input`, and binds it to the element's value. If either is changed, the other automatically updates.

An alternative syntax, in which the value is used as the signal name, is also available. This can be useful depending on the templating language you are using.

```html
<input data-bind="input" />
```

### `data-text`

To see this in action, we can use the [`data-text`](/reference/attribute_plugins#data-text) attribute.

```html
<input data-bind-input />
<div data-text="$input">
  I will be replaced with the contents of the input signal
</div>
```

<div class="flex items-start justify-between p-8 alert">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input1 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input1" class="output"></div>
        </div>
    </div>
</div>

This sets the text content of an element to the value of the signal `$input`. The `$` prefix is required to denote a signal.

Note that `data-*`  attributes are evaluated in the order they appear in the DOM, so the `data-text` attribute must come _after_ the `data-bind` attribute. See the [attribute plugins reference](/reference/attribute_plugins) for more information.

The value of the `data-text` attribute is a [Datastar expression](/guide/datastar_expressions) that is evaluated, meaning that we can use JavaScript in it.

```html
<input data-bind-input />
<div data-text="$input.toUpperCase()">
  Will be replaced with the uppercase contents of the input signal
</div>
```

<div class="flex items-start justify-between p-8 alert">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input2 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input2.toUpperCase()" class="output"></div>
        </div>
    </div>
</div>

### `data-computed`

The [`data-computed`](/reference/attribute_plugins#data-computed) attribute creates a new signal that is computed based on a reactive expression. The computed signal is read-only, and its value is automatically updated when any signals in the expression are updated.

```html
<input data-bind-input />
<div data-computed-repeated="$input.repeat(2)">
    <div data-text="$repeated">
        Will be replaced with the contents of the repeated signal
    </div>
</div>
```

This results in the `$repeated` signal's value always being equal to the value of the `$input` signal repeated twice. Computed signals are useful for memoizing expressions containing other signals.

<div class="flex items-start justify-between p-8 alert">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input3 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-computed-repeated="$input3.repeat(2)" data-text="$repeated" class="output"></div>
        </div>
    </div>
</div>

### `data-show`

The [`data-show`](/reference/attribute_plugins#data-show) attribute can be used to show or hide an element based on whether an expression evaluates to `true` or `false`.

```html
<input data-bind-input />
<button data-show="$input != ''">Save</button>
```

This results in the button being visible only when the input is _not_ an empty string (this could also be written as `!input`).

<div class="flex items-start justify-between p-8 alert">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input4 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input4" class="output"></div>
        </div>
    </div>
    <button data-show="$input4 != ''" class="btn btn-primary">
        Save
    </button>
</div>

### `data-class`

The [`data-class`](/reference/attribute_plugins#data-class) attribute allows us to add or remove a class to or from an element based on an expression.

```html
<input data-bind-input />
<button data-class-hidden="$input == ''">Save</button>
```

If the expression evaluates to `true`, the `hidden` class is added to the element; otherwise, it is removed.

<div class="flex items-start justify-between p-8 alert">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input5 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input5" class="output"></div>
        </div>
    </div>
    <button data-class-hidden="$input5 == ''" class="btn btn-primary">
        Save
    </button>
</div>

The `data-class` attribute can also be used to add or remove multiple classes from an element using a set of key-value pairs, where the keys represent class names and the values represent expressions.

```html
<button data-class="{hidden: $input == '', 'font-bold': $input == 1}">Save</button>
```

### `data-attr`

The [`data-attr`](/reference/attribute_plugins#data-attr) attribute can be used to bind the value of any HTML attribute to an expression.

```html
<input data-bind-input />
<button data-attr-disabled="$input == ''">Save</button>
```

This results in a `disabled` attribute being given the value `true` whenever the input is an empty string.

<div class="flex items-start justify-between p-8 alert" data-signals-input6="''">
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input6 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input6" class="output"></div>
        </div>
    </div>
    <button data-attr-disabled="$input6 == ''" class="btn btn-primary">
        Save
    </button>
</div>

The `data-attr` attribute can also be used to set the values of multiple attributes on an element using a set of key-value pairs, where the keys represent attribute names and the values represent expressions.

```html
<button data-attr="{disabled: $input == '', title: $input}">Save</button>
```

### `data-signals`

So far, we've created signals on the fly using `data-bind` and `data-computed`. All signals are merged into a global set of signals that are accessible from anywhere in the DOM.

We can also create signals using the [`data-signals`](/reference/attribute_plugins#data-signals) attribute.

```html
<div data-signals-input="1"></div>
```

Using `data-signals` _merges_ one or more signals into the existing signals. Values defined later in the DOM tree override those defined earlier.

Signals can be namespaced using dot-notation.

```html
<div data-signals-form.input="2"></div>
```

The `data-signals` attribute can also be used to merge multiple signals using a set of key-value pairs, where the keys represent signal names and the values represent expressions.

```html
<div data-signals="{input: 1, form: {input: 2}}"></div>
```

### `data-on`

The [`data-on`](/reference/attribute_plugins#data-on) attribute can be used to attach an event listener to an element and execute an expression whenever the event is triggered.

```html
<input data-bind-input />
<button data-on-click="$input = ''">Reset</button>
```

This results in the `$input` signal's value being set to an empty string whenever the button element is clicked. This can be used with any valid event name such as `data-on-keydown`, `data-on-mouseover`, etc.

<div class="flex items-start justify-between p-8 alert" >
    <div class="flex flex-col gap-4">
        <div class="flex items-center">
            <div class="w-20">Input:</div>
            <input data-bind-input7 class="input input-bordered" />
        </div>
        <div class="flex items-center">
            <div class="w-20">Output:</div>
            <div data-text="$input7" class="output"></div>
        </div>
    </div>
    <button data-on-click="$input7 = ''" class="btn btn-secondary">
        Reset
    </button>
</div>

So what else can we do now that we have declarative signals and expressions? Anything we want, really!

See if you can follow the code below based on what you've learned so far, _before_ trying the demo.

```html
<div
  data-signals="{response: '', answer: 'bread'}"
  data-computed-correct="$response.toLowerCase() == $answer"
>
  <div id="question">What do you put in a toaster?</div>
  <button data-on-click="$response = prompt('Answer:') ?? ''">BUZZ</button>
  <div data-show="$response != ''">
    You answered “<span data-text="$response"></span>”.
    <span data-show="$correct">That is correct ✅</span>
    <span data-show="!$correct">
      The correct answer is “
      <span data-text="$answer"></span>
      ” 🤷
    </span>
  </div>
</div>
```

<div data-signals="{response1: '', answer1: 'bread'}" data-computed-correct1="$response1.toLowerCase() == $answer1" class="flex items-start justify-between gap-4 p-8 alert">
    <div class="space-y-3">
        <div id="question1">
            What do you put in a toaster?
        </div>
        <div data-show="$response1 != ''">
            You answered “<span data-text="$response1"></span>”.
            <span data-show="$correct1">That is correct ✅</span>
            <span data-show="!$correct1">
                The correct answer is “<span data-text="$answer1"></span>” 🤷
            </span>
        </div>
    </div>
    <button data-on-click="$response1 = prompt('Answer:') ?? ''" class="btn btn-primary">
        BUZZ
    </button>
</div>

We've just scratched the surface of frontend reactivity. Now let's take a look at how we can bring the backend into play.

## Backend Setup

Datastar uses [Server-Sent Events](https://en.wikipedia.org/wiki/Server-sent_events) (SSE) to stream zero or more events from the web server to the browser. There's no special backend plumbing required to use SSE, just some syntax. Fortunately, SSE is straightforward and [provides us with some advantages](/essays/event_streams_all_the_way_down).

First, set up your backend in the language of your choice. Familiarize yourself with [sending SSE events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#sending_events_from_the_server), or use one of the backend [SDKs](/reference/sdks) to get up and running even faster. We're going to use the SDKs in the examples below, which set the appropriate headers and format the events for us.

The following code would exist in a controller action endpoint in your backend.

!!!CODE_SNIPPET:getting_started/setup!!!

The `mergeFragments()` method merges the provided HTML fragment into the DOM, replacing the element with `id="question"`. An element with the ID `question` must _already_ exist in the DOM.

The `mergeSignals()` method merges the `response` and `answer` signals into the frontend signals.

With our backend in place, we can now use the `data-on-click` attribute to trigger the [`@get()`](/reference/action_plugins#get) action, which sends a `GET` request to the `/actions/quiz` endpoint on the server when a button is clicked.

```html
<div
  data-signals="{response: '', answer: ''}"
  data-computed-correct="$response.toLowerCase() == $answer"
>
  <div id="question"></div>
  <button data-on-click="@get('/actions/quiz')">Fetch a question</button>
  <button
    data-show="$answer != ''"
    data-on-click="$response = prompt('Answer:') ?? ''"
  >
    BUZZ
  </button>
  <div data-show="$response != ''">
    You answered “<span data-text="$response"></span>”.
    <span data-show="$correct">That is correct ✅</span>
    <span data-show="!$correct">
      The correct answer is “<span data-text="$answer"></span>” 🤷
    </span>
  </div>
</div>
```

Now when the `Fetch a question` button is clicked, the server will respond with an event to modify the `question` element in the DOM and an event to modify the `response` and `answer` signals. We're driving state from the backend!

<div data-signals="{response2: '', answer2: ''}" data-computed-correct2="$response2.toLowerCase() == $answer2" class="flex items-start justify-between gap-4 p-8 alert">
    <div class="pb-3 space-y-3">
        <div id="question2"></div>
        <div data-show="$response2 != ''">
            You answered “<span data-text="$response2"></span>”.
            <span data-show="$correct2">That is correct ✅</span>
            <span data-show="!$correct2">
                The correct answer is “<span data-text="$answer2"></span>” 🤷
            </span>
        </div>
        <button data-on-click="@get('/examples/quiz/data')" class="btn btn-secondary">
            Fetch a question
        </button>
    </div>
    <button data-show="$answer2 != ''" data-on-click="$response2 = prompt('Answer:') ?? ''" class="btn btn-primary">
        BUZZ
    </button>
</div>

### `data-indicator`

The [`data-indicator`](/reference/attribute_plugins#data-data-indicator) attribute sets the value of a signal to `true` while the request is in flight, otherwise `false`. We can use this signal to show a loading indicator, which may be desirable for slower responses.

```html
<div id="question"></div>
<button
  data-on-click="@get('/actions/quiz')"
  data-indicator-fetching
>
  Fetch a question
</button>
<div data-class-loading="$fetching" class="indicator"></div>
```

<div data-signals="{response3: '', answer3: ''}" data-computed-correct3="$response3.toLowerCase() == $answer3" class="flex items-start justify-between gap-4 p-8 alert">
    <div class="pb-3 space-y-3">
        <div id="question3"></div>
        <div data-show="$response3 != ''">
            You answered “<span data-text="$response3"></span>”.
            <span data-show="$correct3">That is correct ✅</span>
            <span data-show="!$correct3">
                The correct answer is “<span data-text="$answer3"></span>” 🤷
            </span>
        </div>
        <div class="flex items-center gap-2">
            <button id="fetch-a-question" data-on-click="@get('/examples/quiz_slow/data')" data-indicator-fetching class="btn btn-secondary">
                Fetch a question
            </button>
            <div data-class-loading="$fetching" class="indicator"></div>
        </div>
    </div>
    <button data-show="$answer3 != ''" data-on-click="$response3 = prompt('Answer:') ?? ''" class="btn btn-primary">
        BUZZ
    </button>
</div>

The `data-indicator` attribute can also be written with signal name in the attribute value.

```html
<button
  data-on-click="@get('/actions/quiz')"
  data-indicator="fetching"
>
```

We're not limited to just `GET` requests. Datastar provides [backend plugin actions](/reference/action_plugins#backend-plugins) for each of the methods available: `@get()`, `@post()`, `@put()`, `@patch()` and `@delete()`.

Here's how we could send an answer to the server for processing, using a `POST` request.

```html
<button data-on-click="@post('/actions/quiz')">
  Submit answer
</button>
```

One of the benefits of using SSE is that we can send multiple events (HTML fragments, signal updates, etc.) in a single response.

!!!CODE_SNIPPET:getting_started/multiple_events!!!

## Actions

Actions in Datastar are helper functions that are available in `data-*` attributes and have the syntax `@actionName()`. We already saw the backend plugin actions above. Here are a few other common actions.

### `@setAll()`

The `@setAll()` action sets the values of multiple signals at once. It takes a path prefix that is used to match against signals, and a value to set them to, as arguments.

```html
<button data-on-click="@setAll('form.', true)"></button>
```

This sets the values of all signals namespaced under the `form` signal to `true`, which could be useful for enabling input fields in a form.

```html
<input type="checkbox" data-bind-checkboxes.checkbox1 /> Checkbox 1
<input type="checkbox" data-bind-checkboxes.checkbox2 /> Checkbox 2
<input type="checkbox" data-bind-checkboxes.checkbox3 /> Checkbox 3
<button data-on-click="@setAll('checkboxes.', true)">Check All</button>
```

<div class="flex flex-col items-start gap-2 p-8 alert">
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 1</span>
            <input type="checkbox" data-bind-checkboxes1.checkbox1 class="toggle" />
        </label>
    </div>
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 2</span>
            <input type="checkbox" data-bind-checkboxes1.checkbox2 class="toggle" />
        </label>
    </div>
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 3</span>
            <input type="checkbox" data-bind-checkboxes1.checkbox3 class="toggle" />
        </label>
    </div>
    <button data-on-click="@setAll('checkboxes1.', true)" class="mt-4 btn btn-secondary">
        Check All
    </button>
</div>

### `@toggleAll()`

The `@toggleAll()` action toggles the values of multiple signals at once. It takes a path prefix that is used to match against signals, as an argument.

```html
<button data-on-click="@toggleAll('form.')"></button>
```

This toggles the values of all signals containing `form.` (to either `true` or `false`), which could be useful for toggling input fields in a form.

```html
<input type="checkbox" data-bind-checkboxes.checkbox1 /> Checkbox 1
<input type="checkbox" data-bind-checkboxes.checkbox2 /> Checkbox 2
<input type="checkbox" data-bind-checkboxes.checkbox3 /> Checkbox 3
<button data-on-click="@toggleAll('checkboxes.')">Toggle All</button>
```

<div class="flex flex-col items-start gap-2 p-8 alert">
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 1</span>
            <input type="checkbox" data-bind-checkboxes2.checkbox_1 class="toggle" />
        </label>
    </div>
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 2</span>
            <input type="checkbox" data-bind-checkboxes2.checkbox_2 class="toggle" />
        </label>
    </div>
    <div class="form-control">
        <label class="gap-2 cursor-pointer label">
            <span class="label-text">Checkbox 3</span>
            <input type="checkbox" data-bind-checkboxes2.checkbox_3 class="toggle" />
        </label>
    </div>
    <button data-on-click="@toggleAll('checkboxes2.')" class="mt-4 btn btn-secondary">
        Toggle All
    </button>
</div>

View the [full reference](/reference/overview).
