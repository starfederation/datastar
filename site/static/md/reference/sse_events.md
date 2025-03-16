# SSE Events

Responses to [backend plugin actions](/reference/action_plugins#backend-plugins) must contain zero or more Datastar [SSE](https://en.wikipedia.org/wiki/Server-sent_events) events.

<div class="alert alert-info">
    <iconify-icon icon="simple-icons:rocket"></iconify-icon>
    <div>
        The backend <a href="/reference/sdks">SDKs</a> can handle the formatting of SSE events for you.
    </div>
</div>

## Event Types

### `datastar-merge-fragments`

Merges one or more fragments into the DOM. By default, Datastar merges fragments using [Idiomorph](https://github.com/bigskysoftware/idiomorph), which matches top level elements based on their ID.

```
event: datastar-merge-fragments
data: fragments <div id="foo">Hello, world!</div>
```

Additional `data` lines can be added to the response to override the default behavior.

| Key                                | Description                                                                                                             |
|------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| `data: selector #foo`              | Selects the target element of the `merge` process using a CSS selector.                                                 |
| `data: mergeMode morph`            | Merges the fragment using [Idiomorph](https://github.com/bigskysoftware/idiomorph). This is the default merge strategy. |
| `data: mergeMode inner`            | Replaces the target's innerHTML with the fragment.                                                                      |
| `data: mergeMode outer`            | Replaces the target's outerHTML with the fragment.                                                                      |
| `data: mergeMode prepend`          | Prepends the fragment to the target's children.                                                                         |
| `data: mergeMode append`           | Appends the fragment to the target's children.                                                                          |
| `data: mergeMode before`           | Inserts the fragment before the target as a sibling.                                                                    |
| `data: mergeMode after`            | Inserts the fragment after the target as a sibling.                                                                     |
| `data: mergeMode upsertAttributes` | Merges attributes from the fragment into the target – useful for updating a signals.                                      |
| `data: useViewTransition true`     | Whether to use view transitions when merging into the DOM. Defaults to `false`.                                         |
| `data: fragments`                  | The HTML fragments to merge into the DOM.                                                                               |

Sample output showing all options:

```
event: datastar-merge-fragments
data: selector #foo
data: mergeMode append
data: useViewTransition true
data: fragments <div>
data: fragments Hello, world!
data: fragments </div>
```

### `datastar-merge-signals`

Updates the signals with new values. The `onlyIfMissing` line determines whether to update the signals with new values only if the key does not exist. The `signals` line should be a valid `data-signals` attribute. This will get merged into the signals.

Sample output showing all options:

```
event: datastar-merge-signals
data: onlyIfMissing false
data: signals {foo: 1}
```

### `datastar-remove-fragments`

Removes one or more HTML fragments that match the provided selector from the DOM.

Sample output:

```
event: datastar-remove-fragments
data: selector #foo
```

### `datastar-remove-signals`

Removes signals that match one or more provided paths.

Sample output:

```
event: datastar-remove-signals
data: paths foo.bar
data: paths baz
```

### `datastar-execute-script`

Executes JavaScript in the browser. The `autoRemove` line determines whether to remove the script after execution. Each `attributes` line adds an attribute (in the format `name value`) to the `script` element. Each `script` line contains JavaScript to be executed by the browser.

Sample output showing all options:

```
event: datastar-execute-script
data: autoRemove true
data: attributes type module
data: attributes defer true
data: script console.log('Hello, world!')
data: script console.log('A second greeting')
```