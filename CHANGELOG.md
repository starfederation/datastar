# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-RC.12

Some plugins are now available under [Datastar Pro](https://data-star.dev/reference/datastar_pro), which adds functionality to the the free open source Datastar framework. These plugins are available under a commercial license that helps fund our open source work.

Of the many changes listed below, one major feature is that **objects in signals are now reactive**! This means that you can now create complex data structures in signals, and any changes to these objects will automatically propogate to expressions.

SSE event handling has also changed, in addition to all of the **SDKs**. Please refer to the SSE docs and each of the SDKs for the correct syntax to use.

- Objects in signals are now reactive, meaning that any changes to these objects will automatically propogate to expressions.
- Plugins are now reapplied on morph only if their values/keys/modifiers have changed.
- Added the ability for Datastar to receive `text/html`, `application/json`, and `text/javascript` content types, that patch elements, patch signals, and execute JavaScript respectively.
- Added a `data-effect` attribute that executes an expression when any of the signals it references change.
- Added a `data-json-signals` attribute that sets the text content of an element to a reactive JSON stringified version of all signals.
- Added a `data-ignore-morph` attribute to the `PatchElements` watcher that skips morphing the respective element and its children.
- Added a `data-preserve-attr` attribute that preserves the client side state of an attribute through a morph.
- Added a `data-scope` attribute that allows setting a scope for signals.
- Added a `data-on-resize` attribute (PRO) that attaches a [ResizeObserver](https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver) to the element, and executes the expression each time the element’s dimensions change.
- Added a `data-query-string` attribute (PRO) that syncs the query string with signal values, including optional history support.
- Added a `data-on-signal-patch` attribute (PRO) that executes an expression when a signal patch takes place.
- Added a `data-on-signal-patch-filter` attribute (PRO) for filtering the signals that cause the expression in `data-on-signal-patch` to be executed.
- Added a `datastar-upload-progress` event (PRO) for monitoring file upload progress.
- Added a `filterSignals` option to SSE actions that filters the signals send to the backend based on include and exclude regular expression.
- Added a `__scoped` modifier that scopes signals created by an attribute to the closest defined scope.
- Added a `__trust` modifier to the `data-on` attribute, which runs the expression even if the [`isTrusted`](https://developer.mozilla.org/en-US/docs/Web/API/Event/isTrusted) property on the event is `false`.
- Added an `isExpr` flag to attribute plugins to indicate whether an attribute’s value is an expression or not.
- The URL passed into SSE actions (`@get`, `@post`, etc.) is now treated as a relative URI.
- The default `Content-Type` header sent with `form` requests is now `application/x-www-form-urlencoded`.
- The value of a clicked button element is now included in the request when using the `form` content type.
- The `data-star-ignore` attribute has been renamed `data-ignore`.
- The `data-attr` attribute now renders `true` as `""` instead of `"true"` (e.g. `checked=""` instead of `checked="true"`).
- The `data-attr` attribute now preserves the string literals `"false"`, `"null"`, and `"undefined"` when using a key.
- Fixed a bug when using the `__debounce.leading` modifier with the `data-on` attribute.
- Removed the `data-on-signal-change` attribute. Use the new `data-on-signal-patch` attribute instead.
- Removed the `datastar-signal-change` event. Use the new `datastar-signal-patch` event instead.
- Removed the `includeLocal` option in backend action requests. Use the `filterSignals` option instead.
- Removed the variable `ctx` from data attributes. Use the new `el` variable to access the element the attribute is attached to, use the new `$` variable to access the signal root, or the `data-json-signals` attribute to output all signals.
- Removed the auto generated IDs that were assigned to elements using data attributes.
- Removed support for adding a dollar sign prefix to signal names in the value of the `data-bind`, `data-ref`, and `data-indicator` attributes.

## Changes to SSE Event Handling

- Renamed the `MergeFragments` and `MergeSignals` watchers to `PatchElements` and `PatchSignals` respectively.
- Renamed the `mergeMode` option of the `PatchElements` watcher to `mode`.
- Renamed the `morph` mode to `outer`.
- Renamed the `outer` mode to `replace`.
- The `inner` mode now morphs the element’s inner HTML.
- Removed the `upsertAttributes` mode.
- Added the `remove` mode.
- The `PatchSignals` watcher now patches (adds/updates/removes) signals according to the <a href="https://datatracker.ietf.org/doc/rfc7396/" target="_blank" rel="noopener noreferrer">JSON Merge Patch RFC 7396</a>.
- Removed the `RemoveFragments`, `RemoveSignals`, and `ExecuteScript` watchers.